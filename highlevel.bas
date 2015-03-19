'' Higher level code transformations

#include once "fbfrog.bi"

namespace hl
	dim shared symbols as THASH

	'' Used by both typedef expansion and forward declaration addition passes
	'' hlExpandSpecialTypedefs: data = typedef ASTNODE, needs freeing
	'' hlCollectForwardUses/hlAddForwardDecls: data = hl.types array index
	dim shared typehash as THASH

	'' data = new name, needs freeing
	dim shared renamejobs as THASH

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
	function = cint(hashLookupDataOrNull(@hl.symbols, id))
end function

private sub hlCollectSymbol(byval id as zstring ptr, byval dtype as integer)
	select case dtype
	case TYPE_UDT, TYPE_PROC
		'' Don't bother tracking complex types - not worth it yet
		dtype = TYPE_NONE
	end select
	hashAddOverwrite(@hl.symbols, id, cptr(any ptr, dtype))
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
private function typeCBop(byval astclass as integer, byval a as integer, byval b as integer) as integer
	'' Logic/relational operations: result always is a 32bit int
	select case astclass
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
	end select

	'' Math/bitwise operations: depends on operands

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

private function hIsIifBopUop(byval astclass as integer) as integer
	'' IIF, BOPs, UOPs
	select case as const astclass
	case ASTCLASS_IIF, _
	     ASTCLASS_CLOGOR, ASTCLASS_CLOGAND, _
	     ASTCLASS_CEQ, ASTCLASS_CNE, ASTCLASS_CLT, _
	     ASTCLASS_CLE, ASTCLASS_CGT, ASTCLASS_CGE, _
	     ASTCLASS_OR, ASTCLASS_XOR, ASTCLASS_AND, _
	     ASTCLASS_SHL, ASTCLASS_SHR, _
	     ASTCLASS_ADD, ASTCLASS_SUB, _
	     ASTCLASS_MUL, ASTCLASS_DIV, ASTCLASS_MOD, _
	     ASTCLASS_CLOGNOT, ASTCLASS_NOT, _
	     ASTCLASS_NEGATE, ASTCLASS_UNARYPLUS, _
	     ASTCLASS_CDEFINED, ASTCLASS_SIZEOF
		function = TRUE
	end select
end function

private sub hlCalculateCTypes(byval n as ASTNODE ptr)
	'' No types should be set yet, except for type casts and atoms
	#if __FB_DEBUG__
		select case n->class
		case ASTCLASS_CAST, ASTCLASS_DATATYPE, _
		     ASTCLASS_CONSTI, ASTCLASS_CONSTF, _
		     ASTCLASS_STRING, ASTCLASS_CHAR
			assert(n->dtype <> TYPE_NONE)
		case else
			if hIsIifBopUop(n->class) then
				assert(n->dtype = TYPE_NONE)
			end if
		end select
	#endif

	scope
		var i = n->head
		while i
			hlCalculateCTypes(i)
			i = i->next
		wend
	end scope

	if hIsIifBopUop(n->class) then
		'' IIF and BOPs will have two operands (ldtype/rdtype will be different),
		'' UOPs will have one operand (i.e. ldtype/rdtype will be the same)
		var ldtype = n->head->dtype
		var rdtype = n->tail->dtype
		n->dtype = typeCBop(n->class, ldtype, rdtype)
	elseif n->class = ASTCLASS_TEXT then
		n->dtype = hlLookupSymbolType(n->text)
	end if
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

	if hIsIifBopUop(n->class) then
		'' Wrap uint32 operation in a cast, unless there already is a cast
		'' TODO: don't add cast if n is a cast
		if (n->dtype = TYPE_ULONG) and (not hIsUlongCast(parent)) then
			n = astNew(ASTCLASS_CAST, n)
			n->dtype = TYPE_ULONG
		end if
	end if

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
		if hIsIifBopUop(n->class) or (n->class = ASTCLASS_TEXT) then
			astSetType(n, TYPE_NONE, NULL)
		end if
	end if
end sub

private function hlFixExpressions(byval n as ASTNODE ptr) as integer
	if n->expr then
		hlCalculateCTypes(n->expr)

		n->expr = hlAddMathCasts(NULL, n->expr)

		'' TODO: shouldn't assume is_bool_context=TRUE for #define bodies
		n->expr = astOpsC2FB(n->expr, (n->class = ASTCLASS_PPDEFINE))

		'' Forget about types again, for better merging (on one side
		'' an identifier may have unknown type, on the other side it
		'' could be known - but they should still be merged)
		hlRemoveExpressionTypes(n->expr)
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

private sub hlApplyRemoveProcOptions(byval ast as ASTNODE ptr)
	var i = ast->head
	while i
		var nxt = i->next

		if i->class = ASTCLASS_PROC then
			if hashContains(@frog.idopt(OPT_REMOVEPROC), i->text, hashHash(i->text)) then
				astRemove(ast, i)
			end if
		end if

		i = nxt
	wend
end sub

private function hlSetArraySizes(byval n as ASTNODE ptr) as integer
	if (n->text <> NULL) and (n->array <> NULL) then
		'' 1 dimension?
		if n->array->head = n->array->tail then
			var dimension = n->array->head
			assert(dimension->class = ASTCLASS_DIMENSION)
			if dimension->expr->class = ASTCLASS_ELLIPSIS then
				dim size as zstring ptr = hashLookupDataOrNull(@frog.setarraysizeoptions, n->text)
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
	dim as ASTNODE ptr renameinfo = hashLookupDataOrNull(@frog.renameopt(opt), n->text)
	if renameinfo then
		assert(*n->text = *renameinfo->alias)
		astRenameSymbol(n, renameinfo->text)
		function = TRUE
	end if
end function

private function hlApplyRenameOption(byval n as ASTNODE ptr) as integer
	static inside_macro as integer

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

	case ASTCLASS_TYPEDEF
		hApplyRenameOption(OPT_RENAMETYPEDEF, n)

	case ASTCLASS_PPDEFINE
		hApplyRenameOption(OPT_RENAMEDEFINE, n)

		if frog.renameopt(OPT_RENAMEMACROPARAM).count > 0 then
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

	case ASTCLASS_TEXT
		hApplyRenameOption(OPT_RENAMEDEFINE, n)
		if inside_macro then
			hApplyRenameOption(OPT_RENAMEMACROPARAM, n)
		end if

	end select

	function = TRUE
end function

private sub hOopsCantExpandTypedef(byval typedef as ASTNODE ptr, byval n as ASTNODE ptr)
	oops("can't solve out " + astDumpPrettyDecl(typedef) + " in " + astDumpPrettyDecl(n))
end sub

private sub hExpandArrayTypedef(byval typedef as ASTNODE ptr, byval n as ASTNODE ptr)
	'' Pointer to array?
	if typeGetPtrCount(n->dtype) > 0 then
		'' FB doesn't support pointers to arrays either, but we can drop the array type,
		'' such that the pointer only points to "single array element". That's pretty much
		'' the best translation we can do here, and better than nothing...
		astSetType(n, typeExpand(n->dtype, typedef->dtype), typedef->subtype)
		if n->dtype = TYPE_NONE then
			hOopsCantExpandTypedef(typedef, n)
		end if
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
private sub hExpandProcTypedef(byval typedef as ASTNODE ptr, byval n as ASTNODE ptr)
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
			hOopsCantExpandTypedef(typedef, n)
		end if
	end select

	'' Insert the typedef's type, overwriting the use of the typedef
	assert(typeGetDtAndPtr(typedef->dtype) = TYPE_PROC)
	astSetType(n, typeUnsetBaseConst(typeSetDt(n->dtype, TYPE_PROC)), typedef->subtype)
end sub

private sub hlExpandSpecialTypedefs(byval n as ASTNODE ptr)
	'' Declaration using one of the special typedefs?
	if typeGetDt(n->dtype) = TYPE_UDT then
		assert(astIsTEXT(n->subtype))
		dim as ASTNODE ptr typedef = hashLookupDataOrNull(@hl.typehash, n->subtype->text)
		if typedef then
			'' Expand the typedef into the declaration.
			'' Since typedefs are also processed this way here, we can be sure that
			'' typedefs themselves are already fully expanded when reaching another
			'' declaration using one of these typedefs. Thus, expanding a typedef
			'' doesn't cause a need for further expansions.
			if typedef->array then
				hExpandArrayTypedef(typedef, n)
			else
				var typedefdtype = typedef->dtype
				select case typeGetDtAndPtr(typedefdtype)
				case TYPE_PROC
					hExpandProcTypedef(typedef, n)
				case TYPE_ZSTRING, TYPE_WSTRING
					astSetType(n, typeExpand(n->dtype, typedefdtype), NULL)
					assert(n->dtype <> TYPE_NONE)
					assert(n->subtype = NULL)
				end select
			end if
		end if
	end if

	if n->subtype then hlExpandSpecialTypedefs(n->subtype)
	if n->array   then hlExpandSpecialTypedefs(n->array  )
	if n->expr    then hlExpandSpecialTypedefs(n->expr   )

	var i = n->head
	while i
		var nxt = i->next

		hlExpandSpecialTypedefs(i)

		if i->class = ASTCLASS_TYPEDEF then
			var dtype = typeGetDtAndPtr(i->dtype)
			var is_array_or_proc = (i->array <> NULL) or (dtype = TYPE_PROC)
			var is_string = (dtype = TYPE_ZSTRING) or (dtype = TYPE_WSTRING)

			if is_array_or_proc or is_string then
				if is_array_or_proc then
					'' Array/proc typedefs can't be preserved for FB;
					'' remove from the AST, hl.typehash will free it later.
					astUnlink(n, i)
				else
					'' Others can be preserved - make a copy for the hl.typehash
					'' so we we'll free that instead of the node in the AST later.
					i = astClone(i)
				end if

				'' TODO: handle duplicate typedefs? (perhaps warn about them?)
				hashAddOverwrite(@hl.typehash, i->text, i)
			end if
		end if

		i = nxt
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

private sub hTurnStringIntoByte(byval n as ASTNODE ptr)
	'' Turn zstring/wstring into byte/wchar_t, but preserve CONSTs
	if typeGetDtAndPtr(n->dtype) = TYPE_ZSTRING then
		n->dtype = typeGetConst(n->dtype) or TYPE_BYTE
	else
		n->dtype = typeGetConst(n->dtype) or TYPE_WCHAR_T
	end if
	if n->subtype then
		'' in case it was a fix-len string
		astDelete(n->subtype)
		n->subtype = NULL
	end if
end sub

private sub hFixCharWchar(byval n as ASTNODE ptr)
	'' Affected by -nostring?
	if n->text then
		if hashContains(@frog.idopt(OPT_NOSTRING), n->text, hashHash(n->text)) then
			hTurnStringIntoByte(n)
			exit sub
		end if
	end if

	'' zstring + array? => fix-len zstring
	if n->array then
		'' Use the last (inner-most) array dimension as the fixed-length string size
		var d = n->array->tail
		assert(d->class = ASTCLASS_DIMENSION)
		assert(d->expr)
		assert(n->subtype = NULL)
		n->subtype = astClone(d->expr)
		astRemove(n->array, d)

		'' If no dimensions left, remove the array type entirely
		if n->array->head = NULL then
			astDelete(n->array)
			n->array = NULL
		end if

		exit sub
	end if

	if n->class <> ASTCLASS_TYPEDEF then
		hTurnStringIntoByte(n)
	end if
end sub

''
'' Handle declarations with plain zstring/wstring type (originally char/wchar_t).
''
'' If it's a char pointer or array, then it's probably supposed to be a string,
'' and we can leave it as-is (zstring/wstring). If it's just a char, then it's
'' probably supposed to be a byte (or wchar_t) though.
''
'' FB doesn't allow "foo as zstring" - it must be a pointer or fixed-length string,
'' except in typedefs. Typedefs are a special case - char/wchar_t means byte/wchar_t
'' or zstring/wstring depending on where they're used. Because of this we expand
'' these typedefs like array/proc typedefs (see hlExpandSpecialTypedefs). To allow
'' this expansion to work, we keep the zstring/wstring type on the typedefs.
''
private function hlFixCharWchar(byval n as ASTNODE ptr) as integer
	if n->class <> ASTCLASS_STRING then
		select case typeGetDtAndPtr(n->dtype)
		case TYPE_ZSTRING, TYPE_WSTRING
			hFixCharWchar(n)
		end select
	end if
	function = TRUE
end function

private sub hlRenameJobsBegin()
	hashInit(@hl.renamejobs, 6, TRUE)
end sub

private function hlRenameJobAdd(byval oldid as zstring ptr, byval newid as zstring ptr) as integer
	newid = strDuplicate(newid)

	var hash = hashHash(oldid)
	var item = hashLookup(@hl.renamejobs, oldid, hash)
	if item->s then
		'' Allow overwriting duplicate typedefs (if both oldid/newid are the same),
		'' but not if the newid differs (then it's a separate typedef which has to be
		'' preserved - we can't rename A => C when already renaming A => B).
		dim as zstring ptr prevnewid = item->data
		if ucase(*prevnewid, 1) <> ucase(*newid, 1) then
			return FALSE
		end if

		'' Free existing newid if there already is a renamejob for this oldid,
		'' then the new newid can be stored
		deallocate(item->data)
		item->data = newid
	else
		hashAdd(@hl.renamejobs, item, hash, oldid, newid)
	end if

	function = TRUE
end function

private sub hMaybeApplyRenameJob(byval n as ASTNODE ptr)
	'' Is there a renamejob for this oldid?
	dim as zstring ptr newid = hashLookupDataOrNull(@hl.renamejobs, n->text)
	if newid then
		'' Change the id if needed
		if *n->text <> *newid then
			astSetText(n, newid)
		end if
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
		for i as integer = 0 to hl.renamejobs.room - 1
			var item = hl.renamejobs.items + i
			if item->s then
				deallocate(item->data)
			end if
		next
	end scope
	hashEnd(@hl.renamejobs)
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
					var newid = i->text
					var oldid = i->subtype->text
					if hlRenameJobAdd(oldid, newid) then
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
				var newid = i->text
				var oldid = i->subtype->text
				if ucase(*oldid, 1) = ucase(*newid, 1) then
					if hlRenameJobAdd(oldid, newid) then
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
	var item = hashLookup(@hl.typehash, id, hash)
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
			.id = id
			.definition = NULL
			.forwarduse = FALSE
			.firstuse = NULL
		end with
		hashAdd(@hl.typehash, item, hash, id, cptr(any ptr, hl.typecount))
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
			function = hashContains(@frog.idopt(OPT_ADDFORWARDDECL), typ.id, hashHash(typ.id))
		end if
	end if
end function

'' Add forward decls for forward-referenced types
private sub hlAddForwardDecls(byval ast as ASTNODE ptr)
	for i as integer = hl.typecount - 1 to 0 step -1
		var typ = hl.types + i
		if hShouldAddForwardDeclForType(*typ) then
			if typ->definition then
				typ->definition->attrib or= ASTATTRIB_FORWARDDECLARED
			end if
			var fwd = astNew(ASTCLASS_FORWARDDECL, typ->id)
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
		function = (n->array->head = n->array->tail) and (n->array->head->expr->class = ASTCLASS_ELLIPSIS)
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

		astRenameSymbol(n, tempid)
		n->attrib or= ASTATTRIB_NORENAMELIST
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

			astRenameSymbol(n, tempid)
			n->attrib or= ASTATTRIB_NORENAMELIST
			hTurnStringIntoByte(n)
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

'' Remap .h name to a .bi name. If it's a known system header, we can even remap
'' it to the corresponding FB header (in some cases it's not as simple as
'' replacing .h by .bi).
private sub hTranslateHeaderFileName(byref filename as string)
	filename = pathStripExt(filename)

	'' Is it one of the CRT headers? Remap it to crt/*.bi
	if hashContains(@fbcrtheaderhash, filename, hashHash(filename)) then
		filename = "crt/" + filename
	end if

	filename += ".bi"
end sub

private sub hlHandleIncludes(byval ast as ASTNODE ptr)
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

	'' For each #include at the top, apply -removeinclude and remap *.h => *.bi
	i = ast->head
	while i <> top
		var nxt = i->next

		'' #include?
		if i->class = ASTCLASS_PPINCLUDE then
			var filename = *i->text
			if hashContains(@frog.removeinclude, filename, hashHash(filename)) then
				astRemove(ast, i)
			else
				hTranslateHeaderFileName(filename)
				astSetText(i, filename)
			end if
		end if

		i = nxt
	wend

	'' Remove duplicate #includes
	dim includes as THASH
	hashInit(@includes, 6, TRUE)

	i = ast->head
	while i <> top
		var nxt = i->next

		'' #include?
		if i->class = ASTCLASS_PPINCLUDE then
			var hash = hashHash(i->text)
			var item = hashLookup(@includes, i->text, hash)
			if item->s then
				astRemove(ast, i)
			else
				hashAdd(@includes, item, hash, i->text, NULL)
			end if
		end if

		i = nxt
	wend

	hashEnd(@includes)
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

		if (i->text <> NULL) and (i->alias <> NULL) and ((i->attrib and ASTATTRIB_NORENAMELIST) = 0) then
			astAppend(list, astNewTEXT( _
				astDumpPrettyClass(i->class) + " " + *i->alias + " => " + *i->text))
		end if

		'' TODO: recurse into struct/union/enum if renaming fields/enumconsts...
		''astAppend(list, hlBuildRenameList(i))

		i = i->next
	wend

	function = list
end function

''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''

private function hIsPpIfBegin(byval n as ASTNODE ptr) as integer
	select case n->class
	case ASTCLASS_PPIF, ASTCLASS_PPELSEIF, ASTCLASS_PPELSE
		function = TRUE
	end select
end function

private function hIsPpIfEnd(byval n as ASTNODE ptr) as integer
	select case n->class
	case ASTCLASS_PPELSEIF, ASTCLASS_PPELSE, ASTCLASS_PPENDIF
		function = TRUE
	end select
end function

private function hIsCompound(byval n as ASTNODE ptr) as integer
	select case n->class
	case ASTCLASS_STRUCT, ASTCLASS_UNION, ASTCLASS_ENUM
		function = TRUE
	case ASTCLASS_PROC
		'' Procedure with body?
		function = (n->expr <> NULL)
	end select
end function

private function hShouldSeparate _
	( _
		byval a as ASTNODE ptr, _
		byval b as ASTNODE ptr _
	) as integer

	if (a->class = ASTCLASS_DIVIDER) or _
	   (b->class = ASTCLASS_DIVIDER) then
		exit function
	end if

	if hIsPpIfBegin(a) and hIsPpIfEnd(b) then
		exit function
	end if

	function = (a->class <> b->class) or _
	           hIsCompound(a) or hIsCompound(b)
end function

''
'' Insert DIVIDERs between statements of different kind, e.g. all #defines in
'' a row shouldn't be divided, but a #define should be divided from a typedef.
'' Structs/unions/enums should always be separated by a divider because they're
'' compounds and normally span multiple lines themselves.
''
sub astAutoAddDividers(byval code as ASTNODE ptr)
	var i = code->head
	while i
		var nxt = i->next

		astAutoAddDividers(i)

		if nxt then
			if hShouldSeparate(i, nxt) then
				astInsert(code, astNew(ASTCLASS_DIVIDER), nxt)
			end if
		end if

		i = nxt
	wend
end sub

sub astPrependMaybeWithDivider(byval group as ASTNODE ptr, byval n as ASTNODE ptr)
	if group->head andalso hShouldSeparate(n, group->head) then
		astPrepend(group, astNew(ASTCLASS_DIVIDER))
	end if
	astPrepend(group, n)
end sub

''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''

''
'' Global (entire AST of an API) highlevel transformations, intended to run 1st
''
'' If any declarations are added to the AST here, care must be taken to set the
'' ASTNODE.location, so they can be assigned to the proper output .bi file.
''
sub hlGlobal(byval ast as ASTNODE ptr)
	hashInit(@hl.symbols, 10, TRUE)
	astVisit(ast, @hlFixExpressions)
	hashEnd(@hl.symbols)

	'' Apply -removeproc options, if any
	if frog.idopt(OPT_REMOVEPROC).count > 0 then
		hlApplyRemoveProcOptions(ast)
	end if

	astVisit(ast, @hlSetArraySizes)

	'' Apply -rename* options, if any
	if frog.have_renames then
		astVisit(ast, @hlApplyRenameOption)
	end if

	'' Solve out special typedefs
	''  * array/function typedefs, which aren't supported in FB
	''  * char/wchar_t typedefs, which become zstring/wstring in some
	''    places, but byte/wchar_t in others (i.e. it depends on context,
	''    which is why the typedef needs to be expanded)
	'' TODO: If possible, turn function typedefs into function pointer
	'' typedefs instead of solving them out
	hashInit(@hl.typehash, 8, FALSE)
	hlExpandSpecialTypedefs(ast)
	scope
		'' Free the collected typedefs which were unlinked from the main AST
		for i as integer = 0 to hl.typehash.room - 1
			var item = hl.typehash.items + i
			if item->s then
				astDelete(item->data)
			end if
		next
	end scope
	hashEnd(@hl.typehash)

	'' Fix up parameters with certain special types:
	''   function => function pointer
	''   array => pointer to the element type
	''   jmp_buf => jmp_buf ptr (jmp_buf is an array type in C)
	astVisit(ast, @hlFixSpecialParameters)

	'' The C parser turned char/wchar_t into zstring/wstring.
	'' Now turn zstring/wstring into byte/wchar_t where it's obviously
	'' not a string.
	astVisit(ast, @hlFixCharWchar)

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
	hashInit(@hl.typehash, 8, FALSE)
	hl.types = NULL
	hl.typecount = 0
	hl.typeroom = 0
	hlScanForForwardUsedTypes(ast)
	hlAddForwardDecls(ast)
	hashEnd(@hl.typehash)
	deallocate(hl.types)

	if frog.fixunsizedarrays then
		hlFixUnsizedArrays(ast)
	end if

	if frog.disableconstants = FALSE then
		hlTurnDefinesIntoProperDeclarations(ast)
	end if
end sub

''
'' .bi-file-specific highlevel transformations, intended to run on the
'' API-specific ASTs in each output .bi file.
''
sub hlFile(byval ast as ASTNODE ptr, byval inclibs as ASTNODE ptr)
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
			if frog.windowsms then
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
	if frog.have_renames then
		var entries = hlBuildRenameList(ast)
		if entries->head then
			var renamelist = astNew(ASTCLASS_RENAMELIST, "The following symbols have been renamed:")
			astAppend(renamelist, entries)
			astPrepend(ast, astNew(ASTCLASS_DIVIDER))
			astPrepend(ast, renamelist)
		else
			astDelete(entries)
		end if
	end if

	'' Move existing #include statements outside the Extern block
	'' Remap *.h => *.bi
	'' Apply -removeinclude
	hlHandleIncludes(ast)

	'' Add #includes for "crt/long[double].bi" and "crt/wchar.bi" if the
	'' binding uses the clong[double]/wchar_t types
	astVisit(ast, @hlSearchSpecialDtypes)
	if hl.uses_clongdouble then
		astPrependMaybeWithDivider(ast, astNew(ASTCLASS_PPINCLUDE, "crt/longdouble.bi"))
	end if
	if hl.uses_clong then
		astPrependMaybeWithDivider(ast, astNew(ASTCLASS_PPINCLUDE, "crt/long.bi"))
	end if

	if inclibs then
		'' Prepend #inclibs
		astPrependMaybeWithDivider(ast, inclibs)
	end if
end sub
