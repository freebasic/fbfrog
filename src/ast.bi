#include once "common.bi"
#include once "api.bi"
#include once "util.bi"

type HeaderInfo
	title as string
	as FileBuffer ptr licensefile, translatorsfile
end type

enum
	TYPE_NONE = 0
	TYPE_ANY          '' C void
	TYPE_BYTE         '' int8
	TYPE_UBYTE        '' uint8
	TYPE_SHORT        '' int16
	TYPE_USHORT       '' uint16
	TYPE_LONG         '' int32
	TYPE_ULONG        '' uint32
	TYPE_CLONG        '' C long, size varies, see FB crt/long.bi
	TYPE_CULONG       '' C unsigned long
	TYPE_INTEGER      '' FB integer = C ssize_t etc., size varies
	TYPE_UINTEGER     '' FB uinteger = C size_t
	TYPE_LONGINT      '' int64
	TYPE_ULONGINT     '' uint64
	TYPE_SINGLE       '' C float
	TYPE_DOUBLE
	TYPE_CLONGDOUBLE  '' C long double, size varies, see FB crt/longdouble.bi
	TYPE_UDT          '' any other type names (typedefs/structs/enums)
	TYPE_PROC         '' function type
	TYPE_ZSTRING      '' C char pointers/arrays, if not turned into TYPE_BYTE
	TYPE_WSTRING      '' C wchar_t pointers/arrays, if not turned into TYPE_WCHAR_T
	TYPE_WCHAR_T      '' Single wchar
	TYPE__COUNT
end enum

const TYPEMASK_DT    = &b00000000000000000000000011111111  '' 1 byte, enough for TYPE_* enum
const TYPEMASK_PTR   = &b00000000000000000000111100000000  '' 0..15, enough for max. 8 PTRs on a type, like FB
const TYPEMASK_REF   = &b00000000000000000001000000000000  '' 0..1, reference or not?
const TYPEMASK_CONST = &b00000000001111111110000000000000  '' 1 bit per PTR + 1 for the toplevel

const TYPEPOS_PTR    = 8  '' bit where PTR mask starts
const TYPEPOS_REF    = TYPEPOS_PTR + 4
const TYPEPOS_CONST  = TYPEPOS_REF + 1

const TYPEMAX_PTR = 8

#define typeSetDt(dtype, dt) ((dtype and (not TYPEMASK_DT)) or (dt and TYPEMASK_DT))
#define typeSetIsConst(dt) ((dt) or (1 shl TYPEPOS_CONST))
#define typeSetIsRef(dt)   ((dt) or (1 shl TYPEPOS_REF  ))
#define typeIsConstAt(dt, at) (((dt) and (1 shl (TYPEPOS_CONST + (at)))) <> 0)
#define typeIsRef(dt) (((dt) and TYPEMASK_REF) <> 0)
#define typeGetDt(dt) ((dt) and TYPEMASK_DT)
#define typeGetRef(dt) ((dt) and TYPEMASK_REF)
#define typeGetDtAndPtr(dt) ((dt) and (TYPEMASK_DT or TYPEMASK_PTR))
#define typeGetPtrCount(dt) (((dt) and TYPEMASK_PTR) shr TYPEPOS_PTR)
#define typeAddrOf(dt) _
	(((dt) and (TYPEMASK_DT or TYPEMASK_REF)) or _
	 (((dt) and TYPEMASK_PTR) + (1 shl TYPEPOS_PTR)) or _
	 (((dt) and TYPEMASK_CONST) shl 1))
#define typeMultAddrOf(dt, count) _
	((dt and (TYPEMASK_DT or TYPEMASK_REF)) or _
	 ((dt and TYPEMASK_PTR) + (count shl TYPEPOS_PTR)) or _
	 ((dt and TYPEMASK_CONST) shl count))
#define typeGetConst(dt) ((dt) and TYPEMASK_CONST)

declare function typeExpand(byval a as integer, byval b as integer) as integer
declare function typeUnsetBaseConst(byval dtype as integer) as integer
declare function typeGetCLong(byval is_unsigned as integer, byval clong32 as integer) as integer
declare function typeDump(byval dtype as integer) as string

enum
	'' Internal helper nodes
	ASTCLASS_GROUP = 0
	ASTCLASS_VERBLOCK
	ASTCLASS_VEROR
	ASTCLASS_VERAND
	ASTCLASS_VERNUMCHECK
	ASTCLASS_DIVIDER
	ASTCLASS_SCOPEBLOCK
	ASTCLASS_UNKNOWN
	ASTCLASS_FBCODE
	ASTCLASS_RENAMELIST
	ASTCLASS_TITLE

	'' Script helper nodes
	ASTCLASS_DECLAREVERSIONS
	ASTCLASS_DECLAREBOOL
	ASTCLASS_SELECTTARGET
	ASTCLASS_SELECTVERSION
	ASTCLASS_SELECTDEFINE
	ASTCLASS_CASE
	ASTCLASS_CASEELSE
	ASTCLASS_ENDSELECT
	ASTCLASS_OPTION

	'' Declarations/statements
	ASTCLASS_PPINCLUDE
	ASTCLASS_PPDEFINE
	ASTCLASS_PPIF
	ASTCLASS_PPELSEIF
	ASTCLASS_PPELSE
	ASTCLASS_PPENDIF
	ASTCLASS_PRAGMAONCE
	ASTCLASS_INCLIB
	ASTCLASS_UNDEF
	ASTCLASS_STRUCT
	ASTCLASS_UNION
	ASTCLASS_ENUM
	ASTCLASS_TYPEDEF
	ASTCLASS_CONST
	ASTCLASS_VAR
	ASTCLASS_FIELD
	ASTCLASS_PROC
	ASTCLASS_PARAM
	ASTCLASS_MACROPARAM
	ASTCLASS_ARRAY
	ASTCLASS_EXTERNBLOCKBEGIN
	ASTCLASS_EXTERNBLOCKEND
	ASTCLASS_RETURN
	ASTCLASS_ASSIGN
	ASTCLASS_SELFOR
	ASTCLASS_SELFXOR
	ASTCLASS_SELFAND
	ASTCLASS_SELFSHL
	ASTCLASS_SELFSHR
	ASTCLASS_SELFADD
	ASTCLASS_SELFSUB
	ASTCLASS_SELFMUL
	ASTCLASS_SELFDIV
	ASTCLASS_SELFMOD
	ASTCLASS_IFBLOCK
	ASTCLASS_IFPART
	ASTCLASS_ELSEIFPART
	ASTCLASS_ELSEPART
	ASTCLASS_DOWHILE
	ASTCLASS_WHILE

	'' Expression atoms etc.
	ASTCLASS_CONSTI
	ASTCLASS_CONSTF
	ASTCLASS_TEXT
	ASTCLASS_STRING
	ASTCLASS_CHAR
	ASTCLASS_DATATYPE
	ASTCLASS_ELLIPSIS

	'' Expressions

	'' BOPs
	ASTCLASS_CLOGOR
	ASTCLASS_CLOGAND
	ASTCLASS_LOGOR
	ASTCLASS_LOGAND
	ASTCLASS_OR
	ASTCLASS_XOR
	ASTCLASS_AND
	ASTCLASS_CCOMMA
	ASTCLASS_CASSIGN
	ASTCLASS_CSELFOR
	ASTCLASS_CSELFXOR
	ASTCLASS_CSELFAND
	ASTCLASS_CSELFSHL
	ASTCLASS_CSELFSHR
	ASTCLASS_CSELFADD
	ASTCLASS_CSELFSUB
	ASTCLASS_CSELFMUL
	ASTCLASS_CSELFDIV
	ASTCLASS_CSELFMOD
	ASTCLASS_CEQ
	ASTCLASS_CNE
	ASTCLASS_CLT
	ASTCLASS_CLE
	ASTCLASS_CGT
	ASTCLASS_CGE
	ASTCLASS_EQ
	ASTCLASS_NE
	ASTCLASS_LT
	ASTCLASS_LE
	ASTCLASS_GT
	ASTCLASS_GE
	ASTCLASS_SHL
	ASTCLASS_SHR
	ASTCLASS_ADD
	ASTCLASS_SUB
	ASTCLASS_MUL
	ASTCLASS_DIV
	ASTCLASS_MOD
	ASTCLASS_INDEX
	ASTCLASS_MEMBER
	ASTCLASS_MEMBERDEREF
	ASTCLASS_STRCAT

	'' UOPs
	ASTCLASS_CLOGNOT
	ASTCLASS_NOT
	ASTCLASS_NEGATE
	ASTCLASS_UNARYPLUS
	ASTCLASS_CDEFINED
	ASTCLASS_DEFINED
	ASTCLASS_ADDROF
	ASTCLASS_DEREF
	ASTCLASS_STRINGIFY
	ASTCLASS_SIZEOF
	ASTCLASS_CAST

	'' Special expressions
	ASTCLASS_IIF
	ASTCLASS_PPMERGE
	ASTCLASS_CALL
	ASTCLASS_STRUCTINIT
	ASTCLASS_ARRAYINIT
	ASTCLASS_DIMENSION

	ASTCLASS__COUNT
end enum

const ASTATTRIB_LOCAL         = 1 shl 0  '' VAR
const ASTATTRIB_STATIC        = 1 shl 1  '' VAR, PROC (ignored when merging PROCs)
const ASTATTRIB_EXTERN        = 1 shl 2  '' VAR, PROC (removed from PROCs by hPostprocessDeclarator())
const ASTATTRIB_BIN           = 1 shl 3  '' CONSTI
const ASTATTRIB_OCT           = 1 shl 4  '' CONSTI
const ASTATTRIB_HEX           = 1 shl 5  '' CONSTI
const ASTATTRIB_CDECL         = 1 shl 6  '' PROC
const ASTATTRIB_STDCALL       = 1 shl 7  '' PROC
const ASTATTRIB_HIDECALLCONV  = 1 shl 8  '' Whether the calling convention is covered by an Extern block, in which case it doesn't need to be emitted.
const ASTATTRIB_POISONED      = 1 shl 9
const ASTATTRIB_PACKED        = 1 shl 10  '' __attribute__((packed))
const ASTATTRIB_VARIADIC      = 1 shl 11  '' PPDEFINE/MACROPARAM: variadic macros
const ASTATTRIB_PARENTHESIZEDMACROPARAM = 1 shl 12
const ASTATTRIB_TAGID         = 1 shl 13
const ASTATTRIB_GENERATEDID   = 1 shl 14
const ASTATTRIB_DLLIMPORT     = 1 shl 15  '' VAR, PROC (ignored when merging PROCs)
const ASTATTRIB_ENUMCONST     = 1 shl 16
''                            = 1 shl 17
const ASTATTRIB_USED          = 1 shl 18
const ASTATTRIB_IFNDEFDECL    = 1 shl 19
const ASTATTRIB_NOSTRING      = 1 shl 20 '' helper flag used during CharStringPass to mark nodes affected by -nostring
const ASTATTRIB_STRING        = 1 shl 21 '' same for -string

const ASTATTRIB__CALLCONV = ASTATTRIB_CDECL or ASTATTRIB_STDCALL

'' When changing, adjust astClone(), astIsEqual(), astDump*()
'' TODO: pack
type AstNode
	class		as integer  '' ASTCLASS_*
	attrib		as integer  '' ASTATTRIB_*

	'' Identifiers/string literals, or NULL
	text   as zstring ptr '' Symbol name (original or renamed)
	alias  as zstring ptr '' External name (if symbol was renamed, or if given via asm() in C code, etc.)
	origid as zstring ptr '' Original name (if symbol was renamed)

	'' Examples for symbol identifiers:
	'' extern int a         ;                =>  text="a" alias=NULL origid=NULL  =>  extern a as long
	'' extern int a asm("c");                =>  text="a" alias="c" origid=NULL   =>  extern a alias "c" as long
	'' extern int a         ; + -rename a b  =>  text="b" alias="a" origid="a"    =>  extern b alias "a" as long + renamelist entry a => b
	'' extern int a asm("c"); + -rename a b  =>  text="b" alias="c" origid="a"    =>  extern b alias "c" as long + renamelist entry a => b

	'' Data type (vars, fields, params, function results, expressions)
	dtype		as integer
	subtype		as AstNode ptr
	array		as AstNode ptr '' ARRAY holding DIMENSIONs, or NULL
	bits		as AstNode ptr '' bitfields only. TODO: change to simple number, FB doesn't support expressions as bitfield size anyways

	'' Initializers, condition expressions, macro/procedure bodies, ...
	expr		as AstNode ptr

	location	as TKLOCATION

	union
		paramcount	as integer  '' PPDEFINE: -1 = #define m, 0 = #define m(), 1 = #define m(a), ...
		maxalign	as integer  '' STRUCT/UNION: FIELD=N/#pragma pack(N)
		opt		as integer  '' OPTION: OPT_*
		apis		as ApiBits  '' VERBLOCK
		vernum		as integer  '' VERNUMCHECK: index into frog.vernums()
	end union

	'' Linked list of child nodes, operands/fields/parameters/...
	head		as AstNode ptr
	tail		as AstNode ptr
	next		as AstNode ptr
	prev		as AstNode ptr
end type

'' result = boolean = whether to visit this node's children
'' (can be used to skip #define bodies, etc.)
type ASTVISITCALLBACK as function(byval as AstNode ptr) as integer

#define astNewTEXT(text) astNew(ASTCLASS_TEXT, text)
#define astNewDEFINED(id) astNew(ASTCLASS_DEFINED, id)
#define astIsCONSTI(n) ((n)->class = ASTCLASS_CONSTI)
#define astIsVERBLOCK(n) ((n)->class = ASTCLASS_VERBLOCK)
#define astIsVERAND(n) ((n)->class = ASTCLASS_VERAND)
#define astIsVEROR(n)  ((n)->class = ASTCLASS_VEROR)
#define astIsTEXT(n) ((n)->class = ASTCLASS_TEXT)
#define astIsDEFINED(n) ((n)->class = ASTCLASS_DEFINED)
#define astIsPPIF(n) ((n)->class = ASTCLASS_PPIF)
#define astIsPPELSEIF(n) ((n)->class = ASTCLASS_PPELSEIF)
#define astIsPPELSE(n) ((n)->class = ASTCLASS_PPELSE)
#define astIsPPENDIF(n) ((n)->class = ASTCLASS_PPENDIF)
#define astIsNOT(n) ((n)->class = ASTCLASS_NOT)

declare function astNew overload(byval class_ as integer) as AstNode ptr
declare function astNew overload(byval class_ as integer, byval text as zstring ptr) as AstNode ptr
declare function astNew overload(byval class_ as integer, byval c1 as AstNode ptr, byval c2 as AstNode ptr = NULL) as AstNode ptr
declare function astNewPPDEFINE(byval id as zstring ptr) as AstNode ptr
declare function astNewIIF _
	( _
		byval cond as AstNode ptr, _
		byval l as AstNode ptr, _
		byval r as AstNode ptr _
	) as AstNode ptr
declare function astNewGROUP overload() as AstNode ptr
declare function astNewGROUP overload(byval c1 as AstNode ptr, byval c2 as AstNode ptr = NULL) as AstNode ptr
declare sub astBuildGroupAndAppend(byref group as AstNode ptr, byval n as AstNode ptr)
declare function astNewDEFINEDfb64(byval negate as integer) as AstNode ptr
declare function astNewDEFINEDfbarm(byval negate as integer) as AstNode ptr
declare function astNewDEFINEDfbos(byval os as integer) as AstNode ptr
declare function astNewOPTION(byval opt as integer, byval text1 as zstring ptr = NULL, byval text2 as zstring ptr = NULL) as AstNode ptr
declare sub astTakeChildren(byval d as AstNode ptr, byval s as AstNode ptr)
declare sub astTakeAndPrependChildren(byval d as AstNode ptr, byval s as AstNode ptr)
declare sub astTakeAndAppendChildSequence(byval d as AstNode ptr, byval s as AstNode ptr, byval first as AstNode ptr, byval last as AstNode ptr)
declare function astCloneChildren(byval src as AstNode ptr) as AstNode ptr
declare function astGroupContains(byval group as AstNode ptr, byval lookfor as AstNode ptr) as integer
declare function astGroupContainsAnyChildrenOf(byval l as AstNode ptr, byval r as AstNode ptr) as integer
declare function astGroupContainsAllChildrenOf(byval l as AstNode ptr, byval r as AstNode ptr) as integer
declare sub astDelete(byval n as AstNode ptr)
declare sub astInsert(byval parent as AstNode ptr, byval n as AstNode ptr, byval ref as AstNode ptr)
declare sub astPrepend(byval parent as AstNode ptr, byval n as AstNode ptr)
declare sub astAppend(byval parent as AstNode ptr, byval n as AstNode ptr)
declare sub astUnlink(byval parent as AstNode ptr, byval n as AstNode ptr)
declare function astRemove(byval parent as AstNode ptr, byval a as AstNode ptr) as AstNode ptr
declare sub astRemoveChildren(byval parent as AstNode ptr)
declare function astReplace _
	( _
		byval parent as AstNode ptr, _
		byval old as AstNode ptr, _
		byval n as AstNode ptr _
	) as AstNode ptr
declare sub astSetText(byval n as AstNode ptr, byval text as zstring ptr)
declare sub astSetAlias(byval n as AstNode ptr, byval alias_ as zstring ptr)
declare sub astRenameSymbol(byval n as AstNode ptr, byval newid as zstring ptr)
declare sub astRenameSymbolWithoutSettingOrigId(byval n as AstNode ptr, byval newid as zstring ptr)
declare sub astTakeAliasFromId(byval dst as AstNode ptr, byval src as AstNode ptr)
declare sub astTakeOrigId(byval dst as AstNode ptr, byval src as AstNode ptr)
declare sub astTakeAliasAndOrigId(byval dst as AstNode ptr, byval src as AstNode ptr)
declare sub astCopyOrigId(byval dst as AstNode ptr, byval src as AstNode ptr)
declare sub astSetType _
	( _
		byval n as AstNode ptr, _
		byval dtype as integer, _
		byval subtype as AstNode ptr _
	)
declare function astCloneNode(byval n as AstNode ptr) as AstNode ptr
declare function astClone(byval n as AstNode ptr) as AstNode ptr
declare function astContains(byval n as AstNode ptr, byval astclass as integer) as integer
declare function astContainsCAssignments(byval n as AstNode ptr) as integer
declare function astHas1Child(byval n as AstNode ptr) as integer
declare function astHasOnlyChild(byval n as AstNode ptr, byval astclass as integer) as integer
declare function astIsCodeBlock(byval n as AstNode ptr) as integer
declare function astIsCodeScopeBlock(byval n as AstNode ptr) as integer
declare function astIsScopeBlockWith1Stmt(byval n as AstNode ptr) as integer
declare function astIsMergableBlock(byval n as AstNode ptr) as integer
declare function astIsCastTo(byval n as AstNode ptr, byval dtype as integer, byval subtype as AstNode ptr) as integer
declare function astIsEqual _
	( _
		byval a as AstNode ptr, _
		byval b as AstNode ptr, _
		byval is_merge as integer = FALSE _
	) as integer
declare function hGetFbNumberLiteralPrefix(byval attrib as integer) as string
declare function astEvalConstiAsInt64(byval n as AstNode ptr) as longint
declare function astIsConst0(byval n as AstNode ptr) as integer
declare function astLookupMacroParam(byval macro as AstNode ptr, byval id as zstring ptr) as integer
declare function astGetMacroParamByNameIgnoreCase(byval macro as AstNode ptr, byval id as zstring ptr) as AstNode ptr
declare sub astVisit(byval n as AstNode ptr, byval callback as ASTVISITCALLBACK)
declare function astCount(byval n as AstNode ptr) as integer
declare function astDumpPrettyClass(byval astclass as integer) as string
declare function astDumpPrettyDecl(byval n as AstNode ptr, byval show_type as integer = FALSE) as string
declare function astDumpOne(byval n as AstNode ptr) as string
declare sub astDump _
	( _
		byval n as AstNode ptr, _
		byval nestlevel as integer = 0, _
		byref prefix as string = "" _
	)
