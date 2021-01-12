'' AST build up/helper functions

#include once "ast.bi"

#include once "emit.bi"
#include once "tk.bi"
#include once "util-str.bi"
#include once "util.bi"

'' Merge/expand dtype b into dtype a, overwriting a's base dtype, but preserving its ptrs/consts
'' This is useful for expanding typedefs into the context of another dtype.
function typeExpand(byval a as integer, byval b as integer) as integer
	var acount = typeGetPtrCount(a)
	var bcount = typeGetPtrCount(b)
	if acount + bcount > TYPEMAX_PTR then
		return TYPE_NONE
	end if
	if typeIsRef(a) and typeIsRef(b) then
		return TYPE_NONE
	end if
	return typeMultAddrOf(b, acount) or typeGetConst(a) or typeGetRef(a)
end function

function typeUnsetBaseConst(byval dtype as integer) as integer
	function = dtype and (not (1 shl (TYPEPOS_CONST + typeGetPtrCount(dtype))))
end function

function typeGetCLong(byval is_unsigned as integer, byval clong32 as integer) as integer
	if clong32 then
		'' C long => LONG (ok on Windows where C long is always 32bit)
		function = iif(is_unsigned, TYPE_ULONG, TYPE_LONG)
	else
		'' C long => CLONG
		function = iif(is_unsigned, TYPE_CULONG, TYPE_CLONG)
	end if
end function

dim shared datatypenames(0 to TYPE__COUNT-1) as zstring ptr => { _
	@"none"    , _
	@"any"     , _
	@"byte"    , _
	@"ubyte"   , _
	@"short"   , _
	@"ushort"  , _
	@"long"    , _
	@"ulong"   , _
	@"clong"   , _
	@"culong"  , _
	@"integer" , _
	@"uinteger", _
	@"longint" , _
	@"ulongint", _
	@"single"  , _
	@"double"  , _
	@"clongdouble", _
	@"udt"     , _
	@"proc"    , _
	@"zstring" , _
	@"wstring" , _
	@"wchar_t"   _
}

function typeDump(byval dtype as integer) as string
	dim s as string

	if typeIsRef(dtype) then
		s += "byref "
	end if

	var dt = typeGetDt(dtype)
	var ptrcount = typeGetPtrCount(dtype)

	if typeIsConstAt(dtype, ptrcount) then
		s += "const "
	end if

	s += *datatypenames(dt)

	for i as integer = (ptrcount - 1) to 0 step -1
		if typeIsConstAt(dtype, i) then
			s += " const"
		end if

		s += " ptr"
	next

	function = s
end function

''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''

dim shared as zstring ptr astnodename(0 to ...) => _
{ _
	_ '' Internal helper nodes
	@"group"     , _
	@"verblock"  , _
	@"veror"     , _
	@"verand"    , _
	@"vernumcheck", _
	@"divider"   , _
	@"scopeblock", _
	@"unknown"   , _
	@"fbcode"    , _
	@"renamelist", _
	@"title"     , _
	_
	_ '' Script helper nodes
	@"declareversions", _
	@"declarebool"  , _
	@"selecttarget" , _
	@"selectversion", _
	@"selectdefine" , _
	@"case"         , _
	@"caseelse"     , _
	@"endselect"    , _
	@"option"       , _
	_
	_ '' Declarations/statements
	@"ppinclude", _
	@"ppdefine" , _
	@"ppif"     , _
	@"ppelseif" , _
	@"ppelse"   , _
	@"ppendif"  , _
	@"pragmaonce", _
	@"inclib"   , _
	@"undef"    , _
	@"struct"   , _
	@"union"    , _
	@"enum"     , _
	@"typedef"  , _
	@"const"    , _
	@"var"      , _
	@"field"    , _
	@"proc"     , _
	@"param"    , _
	@"macroparam" , _
	@"array"      , _
	@"externbegin", _
	@"externend"  , _
	@"return"   , _
	@"assign"   , _
	@"selfor"   , _
	@"selfxor"  , _
	@"selfand"  , _
	@"selfshl"  , _
	@"selfshr"  , _
	@"selfadd"  , _
	@"selfsub"  , _
	@"selfmul"  , _
	@"selfdiv"  , _
	@"selfmod"  , _
	@"ifblock"  , _
	@"ifpart"   , _
	@"elseifpart", _
	@"elsepart" , _
	@"dowhile"  , _
	@"while"    , _
	_
	_ '' Expression atoms etc.
	@"consti"    , _
	@"constf"    , _
	@"text"      , _
	@"string"    , _
	@"char"      , _
	@"datatype"  , _
	@"ellipsis"  , _
	_
	_ '' BOPs
	@"clogor" , _
	@"clogand", _
	@"logor" , _
	@"logand", _
	@"or"     , _
	@"xor"    , _
	@"and"    , _
	@"ccomma" , _
	@"cassign", _
	@"cselfor", _
	@"cselfxor", _
	@"cselfand", _
	@"cselfshl", _
	@"cselfshr", _
	@"cselfadd", _
	@"cselfsub", _
	@"cselfmul", _
	@"cselfdiv", _
	@"cselfmod", _
	@"ceq"    , _
	@"cne"    , _
	@"clt"    , _
	@"cle"    , _
	@"cgt"    , _
	@"cge"    , _
	@"eq"     , _
	@"ne"     , _
	@"lt"     , _
	@"le"     , _
	@"gt"     , _
	@"ge"     , _
	@"shl"    , _
	@"shr"    , _
	@"add"    , _
	@"sub"    , _
	@"mul"    , _
	@"div"    , _
	@"mod"    , _
	@"index"  , _
	@"member" , _
	@"memberderef", _
	@"strcat" , _
	_
	_ '' UOPs
	@"clognot"    , _
	@"not"        , _
	@"negate"     , _
	@"unaryplus"  , _
	@"cdefined"   , _
	@"defined"    , _
	@"addrof"     , _
	@"deref"      , _
	@"stringify"  , _
	@"sizeof"     , _
	@"cast"       , _
	_
	_ '' Special expressions
	@"iif"       , _
	@"ppmerge"   , _
	@"call"      , _
	@"structinit", _
	@"arrayinit" , _
	@"dimension"   _
}

#assert ubound(astnodename) = ASTKIND__COUNT - 1

function astNew overload(byval kind as integer) as AstNode ptr
	var n = new AstNode
	if n = NULL then
		oops("AstNode memory allocation failed")
	end if
	n->kind = kind
	function = n
end function

function astNew overload(byval kind as integer, byval text as zstring ptr) as AstNode ptr
	var n = astNew(kind)
	n->text = strDuplicate(text)
	function = n
end function

function astNew overload(byval kind as integer, byval c1 as AstNode ptr, byval c2 as AstNode ptr) as AstNode ptr
	var n = astNew(kind)
	n->append(c1)
	n->append(c2)
	function = n
end function

function astNewPPDEFINE(byval id as zstring ptr) as AstNode ptr
	var n = astNew(ASTKIND_PPDEFINE, id)
	n->paramcount = -1
	function = n
end function

function astNewIIF(byval cond as AstNode ptr, byval l as AstNode ptr, byval r as AstNode ptr) as AstNode ptr
	var n = astNew(ASTKIND_IIF, l, r)
	n->expr = cond
	function = n
end function

function astNewGROUP overload() as AstNode ptr
	function = astNew(ASTKIND_GROUP)
end function

function astNewGROUP overload(byval c1 as AstNode ptr, byval c2 as AstNode ptr) as AstNode ptr
	var n = astNewGROUP()
	n->append(c1)
	n->append(c2)
	function = n
end function

sub astBuildGroupAndAppend(byref group as AstNode ptr, byval n as AstNode ptr)
	if group = NULL then group = astNewGROUP()
	group->append(n)
end sub

private function astBuildDEFINED(byval symbol as zstring ptr, byval negate as integer) as AstNode ptr
	var n = astNewDEFINED(symbol)
	if negate then n = astNew(ASTKIND_NOT, n)
	function = n
end function

function astNewDEFINEDfb64(byval negate as integer) as AstNode ptr
	function = astBuildDEFINED("__FB_64BIT__", negate)
end function

function astNewDEFINEDfbarm(byval negate as integer) as AstNode ptr
	function = astBuildDEFINED("__FB_ARM__", negate)
end function

function astNewDEFINEDfbos(byval os as integer) as AstNode ptr
	function = astNewDEFINED(osinfo(os).fbdefine)
end function

function astNewOPTION(byval opt as integer, byval text1 as zstring ptr, byval text2 as zstring ptr) as AstNode ptr
	var n = astNew(ASTKIND_OPTION, text1)
	n->setAlias(text2)
	n->opt = opt
	function = n
end function

function AstNode.getIndexOf(byval lookfor as AstNode ptr) as integer
	var index = 0
	var i = head
	while i
		if i = lookfor then
			return index
		end if
		index += 1
		i = i->nxt
	wend
	function = -1
end function

function AstNode.hasChild(byval lookfor as AstNode ptr) as integer
	return (getIndexOf(lookfor) >= 0)
end function

sub AstNode.takeChildren(byval s as AstNode ptr)
	assert(head = NULL)
	head = s->head
	tail = s->tail
	s->head = NULL
	s->tail = NULL
end sub

sub AstNode.takeAndPrependChildren(byval s as AstNode ptr)
	if s->tail then
		if head then
			s->tail->nxt = head
			head->prev = s->tail
		else
			tail = s->tail
		end if
		head = s->head
		s->head = NULL
		s->tail = NULL
	end if
end sub

sub AstNode.takeAndAppendChildSequence(byval s as AstNode ptr, byval first as AstNode ptr, byval last as AstNode ptr)
	assert(s->getIndexOf(first) <= s->getIndexOf(last))

	'' unlink from s
	if first->prev then
		first->prev->nxt = last->nxt
	else
		s->head = last->nxt
	end if
	if last->nxt then
		last->nxt->prev = first->prev
	else
		s->tail = first->prev
	end if
	first->prev = NULL
	last->nxt = NULL

	'' append to self
	if tail then
		first->prev = tail
		tail->nxt = first
	else
		head = first
	end if
	tail = last
end sub

function AstNode.cloneChildren() as AstNode ptr
	var n = astNewGROUP()
	var i = head
	while i
		n->append(i->clone())
		i = i->nxt
	wend
	function = n
end function

function AstNode.groupContains(byval lookfor as AstNode ptr) as integer
	var i = head
	while i
		if astIsEqual(i, lookfor, FALSE) then
			return TRUE
		end if
		i = i->nxt
	wend
	function = FALSE
end function

function AstNode.groupContainsAnyChildrenOf(byval other as AstNode ptr) as integer
	var i = other->head
	while i
		if groupContains(i) then
			return TRUE
		end if
		i = i->nxt
	wend
end function

function AstNode.groupContainsAllChildrenOf(byval other as AstNode ptr) as integer
	var i = other->head
	while i
		if groupContains(i) = FALSE then
			exit function
		end if
		i = i->nxt
	wend
	function = TRUE
end function

function AstNode.groupsContainEqualChildren(byval other as AstNode ptr) as integer
	function = groupContainsAllChildrenOf(other) and other->groupContainsAllChildrenOf(@this)
end function

destructor AstNode()
	deallocate(text)
	deallocate(alias_)
	deallocate(origid)
	delete subtype
	delete array
	delete bits
	delete expr
	var i = head
	while i
		var nxt = i->nxt
		delete i
		i = nxt
	wend
end destructor

'' Insert in front of ref, or append if ref = NULL
sub AstNode.insert(byval n as AstNode ptr, byval ref as AstNode ptr)
	if n = NULL then exit sub

	assert(not hasChild(n))
	assert(iif(ref, hasChild(ref), TRUE))

	'' If it's a GROUP, insert its children, and delete the GROUP itself
	if n->kind = ASTKIND_GROUP then
		if n->head then
			'' Relink the GROUP's children, so they're added without being reallocated
			if ref then
				if ref->prev then
					ref->prev->nxt = n->head
					n->head->prev = ref->prev
				else
					head = n->head
					assert(n->head->prev = NULL)
				end if
				n->tail->nxt = ref
				ref->prev = n->tail
			else
				if tail then
					tail->nxt = n->head
					n->head->prev = tail
				else
					head = n->head
					assert(n->head->prev = NULL)
				end if
				assert(n->tail->nxt = NULL)
				tail = n->tail
			end if

			n->head = NULL
			n->tail = NULL
		end if

		delete n
		exit sub
	end if

	if ref then
		if ref->prev then
			ref->prev->nxt = n
			n->prev = ref->prev
		else
			head = n
			assert(n->prev = NULL)
		end if
		n->nxt = ref
		ref->prev = n
	else
		if tail then
			tail->nxt = n
			n->prev = tail
		else
			head = n
			assert(n->prev = NULL)
		end if
		assert(n->nxt = NULL)
		tail = n
	end if
end sub

sub AstNode.prepend(byval n as AstNode ptr)
	insert(n, head)
end sub

sub AstNode.append(byval n as AstNode ptr)
	insert(n, NULL)
end sub

sub AstNode.unlink(byval n as AstNode ptr)
	assert(hasChild(n))

	if n->prev then
		n->prev->nxt = n->nxt
	else
		assert(head = n)
		head = n->nxt
	end if

	if n->nxt then
		n->nxt->prev = n->prev
	else
		assert(tail = n)
		tail = n->prev
	end if

	n->prev = NULL
	n->nxt = NULL
end sub

function AstNode.remove(byval n as AstNode ptr) as AstNode ptr
	function = n->nxt
	unlink(n)
	delete n
end function

sub AstNode.removeChildren()
	while head
		remove(head)
	wend
end sub

function AstNode.replace(byval old as AstNode ptr, byval n as AstNode ptr) as AstNode ptr
	assert(old)
	insert(n, old)
	return remove(old)
end function

sub AstNode.setText(byval newtext as zstring ptr)
	deallocate(text)
	text = strDuplicate(newtext)
end sub

sub AstNode.setAlias(byval newalias as zstring ptr)
	deallocate(alias_)
	alias_ = strDuplicate(newalias)
end sub

sub AstNode.renameSymbol(byval newid as zstring ptr)
	if origid = NULL then
		origid = text
		if alias_ = NULL then
			alias_ = strDuplicate(origid)
		end if
	else
		deallocate(text)
	end if
	text = strDuplicate(newid)
end sub

sub AstNode.renameSymbolWithoutSettingOrigId(byval newid as zstring ptr)
	if alias_ = NULL then
		alias_ = strDuplicate(text)
	else
		deallocate(text)
	end if
	text = strDuplicate(newid)
end sub

sub AstNode.takeAliasFromId(byval src as AstNode ptr)
	if alias_ then deallocate(alias_)
	alias_ = src->text
	src->text = NULL
end sub

sub AstNode.takeOrigId(byval src as AstNode ptr)
	if origid then deallocate(origid)
	origid = src->origid
	src->origid = NULL
end sub

sub AstNode.takeAliasAndOrigId(byval src as AstNode ptr)
	if alias_ then deallocate(alias_)
	alias_ = src->alias_
	src->alias_ = NULL
	takeOrigId(src)
end sub

sub AstNode.copyOrigId(byval src as AstNode ptr)
	if origid then deallocate(origid)
	origid = strDuplicate(src->origid)
end sub

sub AstNode.setType(byval dtype as integer, byval subtype as AstNode ptr)
	'' The new subtype may be a child node of the old,
	'' so the new must be cloned before the old is deleted.
	var oldsubtype = this.subtype
	this.subtype = NULL
	this.dtype = dtype
	if subtype then
		this.subtype = subtype->clone()
	end if
	delete oldsubtype
end sub

function AstNode.cloneNode() as AstNode ptr
	var c = astNew(kind)
	c->attrib      = attrib
	c->text        = strDuplicate(text)
	c->alias_      = strDuplicate(alias_)
	c->origid      = strDuplicate(origid)
	c->dtype       = dtype
	if subtype then c->subtype = subtype->clone()
	if array   then c->array   = array->clone()
	if bits    then c->bits    = bits->clone()
	if expr    then c->expr    = expr->clone()
	c->location    = location
	select case kind
	case ASTKIND_PPDEFINE : c->paramcount = paramcount
	case ASTKIND_STRUCT, ASTKIND_UNION : c->maxalign = maxalign
	case ASTKIND_OPTION : c->opt = opt
	case ASTKIND_VERBLOCK, ASTKIND_PPIF, ASTKIND_PPELSEIF
		c->apis = apis
	case ASTKIND_VERNUMCHECK
		c->vernum = vernum
	end select
	function = c
end function

function AstNode.clone() as AstNode ptr
	var c = cloneNode()
	var i = head
	while i
		c->append(i->clone())
		i = i->nxt
	wend
	function = c
end function

function AstNode.contains(byval astkind as integer) as integer
	if kind = astkind then return TRUE
	if subtype then if subtype->contains(astkind) then return TRUE
	if array   then if   array->contains(astkind) then return TRUE
	if bits    then if    bits->contains(astkind) then return TRUE
	if expr    then if    expr->contains(astkind) then return TRUE
	var i = head
	while i
		if i->contains(astkind) then return TRUE
		i = i->nxt
	wend
	function = FALSE
end function

function AstNode.containsCAssignments() as integer
	select case as const kind
	case ASTKIND_CASSIGN, _
	     ASTKIND_CSELFOR, ASTKIND_CSELFXOR, ASTKIND_CSELFAND, _
	     ASTKIND_CSELFSHL, ASTKIND_CSELFSHR, _
	     ASTKIND_CSELFADD, ASTKIND_CSELFSUB, _
	     ASTKIND_CSELFMUL, ASTKIND_CSELFDIV, ASTKIND_CSELFMOD
		return TRUE
	end select
	if subtype then if subtype->containsCAssignments() then return TRUE
	if array   then if   array->containsCAssignments() then return TRUE
	if bits    then if    bits->containsCAssignments() then return TRUE
	if expr    then if    expr->containsCAssignments() then return TRUE
	var i = head
	while i
		if i->containsCAssignments() then return TRUE
		i = i->nxt
	wend
	function = FALSE
end function

function AstNode.has1Child() as integer
	function = head andalso (head = tail)
end function

function AstNode.hasOnlyChild(byval astkind as integer) as integer
	function = has1Child() andalso (head->kind = astkind)
end function

function AstNode.isCodeBlock() as integer
	select case kind
	case ASTKIND_SCOPEBLOCK, ASTKIND_IFBLOCK, ASTKIND_DOWHILE, _
	     ASTKIND_WHILE, ASTKIND_STRUCT, ASTKIND_UNION, ASTKIND_ENUM
		function = TRUE
	case ASTKIND_PROC
		function = (expr <> NULL)
	end select
end function

function AstNode.isCodeScopeBlock() as integer
	select case kind
	case ASTKIND_SCOPEBLOCK, ASTKIND_IFBLOCK, ASTKIND_DOWHILE, ASTKIND_WHILE
		function = TRUE
	end select
end function

function AstNode.isScopeBlockWith1Stmt() as integer
	function = (kind = ASTKIND_SCOPEBLOCK) andalso _
	           has1Child() andalso (not head->isCodeBlock())
end function

function AstNode.isMergableBlock() as integer
	select case kind
	case ASTKIND_STRUCT, ASTKIND_UNION, ASTKIND_ENUM, ASTKIND_RENAMELIST
		function = TRUE
	end select
end function

function AstNode.isCastTo(byval dtype as integer, byval subtype as AstNode ptr) as integer
	function = (kind = ASTKIND_CAST) andalso (this.dtype = dtype) andalso astIsEqual(this.subtype, subtype, FALSE)
end function

''
'' Check whether two ASTs are "equal".
''
'' For the purposes of merging, the rules are somewhat relaxed: two procedure
'' declarations with different calling conventions can be treated as equal if
'' they're both covered by an Extern block, because then we won't emit the
'' calling convention explicitly anyways, and the declarations look exactly the
'' same.
''
'' For VEROR and VERAND nodes the order of children does not matter; we treat
'' them as equal either way.
''
function astIsEqual _
	( _
		byval a as AstNode ptr, _
		byval b as AstNode ptr, _
		byval is_merge as integer _
	) as integer

	if a = b then return TRUE
	if (a = NULL) or (b = NULL) then exit function

	if a->kind <> b->kind then exit function

	var aattrib = a->attrib
	var battrib = b->attrib

	if is_merge then
		'' If callconv is hidden on both sides, then ignore it
		if (aattrib and ASTATTRIB_HIDECALLCONV) and (battrib and ASTATTRIB_HIDECALLCONV) then
			aattrib and= not (ASTATTRIB_CDECL or ASTATTRIB_STDCALL)
			battrib and= not (ASTATTRIB_CDECL or ASTATTRIB_STDCALL)
		end if

		'' Other than that, we should ignore ASTATTRIB_HIDECALLCONV itself,
		'' allowing hFindCommonCallConvsOnMergedDecl() to handle cases where
		'' both sides have the same callconv but different hiding status.
		aattrib and= not ASTATTRIB_HIDECALLCONV
		battrib and= not ASTATTRIB_HIDECALLCONV

		'' Whether nodes are affected by -[no]string only matters during the CharStringPass;
		'' it doesn't matter for merging.
		aattrib and= not (ASTATTRIB_NOSTRING or ASTATTRIB_STRING)
		battrib and= not (ASTATTRIB_NOSTRING or ASTATTRIB_STRING)

		'' Ignore DLLIMPORT/STATIC on procedures for now, as we're not emitting it anyways
		if a->kind = ASTKIND_PROC then
			aattrib and= not (ASTATTRIB_DLLIMPORT or ASTATTRIB_STATIC)
			battrib and= not (ASTATTRIB_DLLIMPORT or ASTATTRIB_STATIC)
		end if
	end if

	'' Some attributes could perhaps always be ignored because they don't cause declarations to really be different.
	'' However, OCT/HEX attributes mustn't be ignored because otherwise number literals with the same digits but
	'' a different base could incorrectly compare equal.

	if aattrib <> battrib then exit function

	if (a->text <> NULL) <> (b->text <> NULL) then exit function
	if a->text then if *a->text <> *b->text then exit function

	if (a->alias_ <> NULL) <> (b->alias_ <> NULL) then exit function
	if a->alias_ then if *a->alias_ <> *b->alias_ then exit function

	if a->dtype <> b->dtype then exit function
	if astIsEqual(a->subtype, b->subtype, is_merge) = FALSE then exit function
	if astIsEqual(a->array, b->array, is_merge) = FALSE then exit function
	if astIsEqual(a->bits, b->bits, is_merge) = FALSE then exit function

	if astIsEqual(a->expr, b->expr, is_merge) = FALSE then exit function

	'' Only check location if not merging: for merging, the source location doesn't matter.
	'' We only use it before the merging step.
	if is_merge = FALSE then
		if a->location.value <> b->location.value then exit function
	end if

	select case a->kind
	case ASTKIND_PPDEFINE
		if a->paramcount <> b->paramcount then exit function

	case ASTKIND_STRUCT, ASTKIND_UNION
		if a->maxalign <> b->maxalign then exit function

	case ASTKIND_OPTION
		if a->opt <> b->opt then exit function

	case ASTKIND_VERBLOCK, ASTKIND_PPIF, ASTKIND_PPELSEIF
		if a->apis.equals(b->apis) = FALSE then exit function

	case ASTKIND_VEROR, ASTKIND_VERAND
		return a->groupsContainEqualChildren(b)

	case ASTKIND_VERNUMCHECK
		if a->vernum <> b->vernum then exit function
	end select

	if is_merge then
		if a->isMergableBlock() then
			return TRUE
		end if
	end if

	'' Children
	a = a->head
	b = b->head
	while (a <> NULL) and (b <> NULL)
		if astIsEqual(a, b, is_merge) = FALSE then
			exit function
		end if
		a = a->nxt
		b = b->nxt
	wend

	'' Both a's and b's last child must be reached at the same time
	function = ((a = NULL) and (b = NULL))
end function

function hGetFbNumberLiteralPrefix(byval attrib as integer) as string
	if attrib and ASTATTRIB_BIN then
		function = "&b"
	elseif attrib and ASTATTRIB_OCT then
		function = "&o"
	elseif attrib and ASTATTRIB_HEX then
		function = "&h"
	end if
end function

function AstNode.evalConstiAsInt64() as longint
	assert(kind = ASTKIND_CONSTI)
	function = vallng(hGetFbNumberLiteralPrefix(attrib) + *text)
end function

function AstNode.isConst0() as integer
	return (kind = ASTKIND_CONSTI) andalso (evalConstiAsInt64() = 0)
end function

function AstNode.lookupMacroParam(byval id as zstring ptr) as integer
	var index = 0
	assert(kind = ASTKIND_PPDEFINE)
	var param = head
	while param
		assert(param->kind = ASTKIND_MACROPARAM)
		if *param->text = *id then
			return index
		end if
		index += 1
		param = param->nxt
	wend
	function = -1
end function

function AstNode.getMacroParamByNameIgnoreCase(byval id as zstring ptr) as AstNode ptr
	assert(kind = ASTKIND_PPDEFINE)
	var param = head
	while param
		assert(param->kind = ASTKIND_MACROPARAM)
		if lcase(*param->text) = lcase(*id) then
			return param
		end if
		param = param->nxt
	wend
	function = NULL
end function

sub AstNode.visit(byval callback as ASTVISITCALLBACK)
	if callback(@this) = FALSE then
		exit sub
	end if

	if subtype then subtype->visit(callback)
	if array   then   array->visit(callback)
	if bits    then    bits->visit(callback)
	if expr    then    expr->visit(callback)

	var i = head
	while i
		i->visit(callback)
		i = i->nxt
	wend
end sub

sub AstNode.visit(byref visitor as AstVisitor)
	if not visitor.visit(this) then
		exit sub
	end if

	if subtype then subtype->visit(visitor)
	if array   then   array->visit(visitor)
	if bits    then    bits->visit(visitor)
	if expr    then    expr->visit(visitor)

	var i = head
	while i
		i->visit(visitor)
		i = i->nxt
	wend
end sub

function AstNode.count() as integer
	var n = 1
	if subtype then n += subtype->count()
	if array   then n +=   array->count()
	if bits    then n +=    bits->count()
	if expr    then n +=    expr->count()
	var i = head
	while i
		n += i->count()
		i = i->nxt
	wend
	return n
end function

''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''
'' AST dumping for pretty output and debugging

function astDumpPrettyKind(byval astkind as integer) as string
	select case astkind
	case ASTKIND_PPDEFINE : function = "#define"
	case ASTKIND_CONST : function = "constant"
	case ASTKIND_VAR   : function = "variable"
	case ASTKIND_PROC  : function = "procedure"
	case else           : function = *astnodename(astkind)
	end select
end function

function AstNode.dumpPrettyDecl(byval show_type as integer) as string
	dim as string s

	if text = NULL then
		s += "anonymous "
	end if

	s += astDumpPrettyKind(kind)

	if text then
		s += " " + strMakePrintable(*text)
	end if

	if alias_ then
		s += " alias """ + strMakePrintable(*alias_) + """"
	end if

	if kind = ASTKIND_PROC then
		s += "()"
	end if

	if show_type then
		if array then
			s += emitFbExpr(array)
		end if
		s += " as " + emitFbType(dtype, subtype)
	end if

	function = s
end function

function AstNode.dumpOne() as string
	dim as string s

	if (kind >= 0) and (kind < ASTKIND__COUNT) then
		s += *astnodename(kind)
	else
		s += "invalid astkind " & kind
	end if

	#macro checkAttrib(a)
		if attrib and ASTATTRIB_##a then s += " " + lcase(#a, 1)
	#endmacro
	checkAttrib(LOCAL)
	checkAttrib(STATIC)
	checkAttrib(EXTERN)
	checkAttrib(OCT)
	checkAttrib(HEX)
	checkAttrib(CDECL)
	checkAttrib(STDCALL)
	checkAttrib(HIDECALLCONV)
	checkAttrib(POISONED)
	checkAttrib(PACKED)
	checkAttrib(VARIADIC)
	checkAttrib(PARENTHESIZEDMACROPARAM)
	checkAttrib(TAGID)
	checkAttrib(GENERATEDID)
	checkAttrib(DLLIMPORT)
	checkAttrib(ENUMCONST)
	checkAttrib(USED)
	checkAttrib(IFNDEFDECL)
	checkAttrib(NOSTRING)
	checkAttrib(STRING)

	select case kind
	case ASTKIND_OPTION
		s += " " + *tkInfoText(opt)
	case ASTKIND_VERBLOCK, ASTKIND_PPIF, ASTKIND_PPELSEIF
		s += " apis=" + apis.dump()
	case ASTKIND_VERNUMCHECK
		s += " vernum=" & vernum
	end select

	if text then
		s += " """ + strMakePrintable(*text) + """"
	end if
	if alias_ then
		s += " alias """ + strMakePrintable(*alias_) + """"
	end if
	if origid then
		s += " origid """ + strMakePrintable(*origid) + """"
	end if

	if dtype <> TYPE_NONE then
		s += " as " + typeDump(dtype)
	end if

	function = s
end function

sub AstNode.dump(byval nestlevel as integer, byref prefix as string)
	nestlevel += 1

	var s = space((nestlevel - 1) * 3)
	if len(prefix) > 0 then
		s += prefix + ": "
	end if
	s += dumpOne()
	print s

	#define dumpField(field) if field then field->dump(nestlevel, #field)
	dumpField(subtype)
	dumpField(array)
	dumpField(bits)
	dumpField(expr)

	var child = head
	while child
		child->dump(nestlevel)
		child = child->nxt
	wend

	nestlevel -= 1
end sub
