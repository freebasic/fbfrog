#include once "api.bi"
#include once "common.bi"
#include once "source.bi"

type HeaderInfo
	title as string
	as SourceInfo ptr licensefile, translatorsfile
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
	ASTKIND_GROUP = 0
	ASTKIND_VERBLOCK
	ASTKIND_VEROR
	ASTKIND_VERAND
	ASTKIND_VERNUMCHECK
	ASTKIND_DIVIDER
	ASTKIND_SCOPEBLOCK
	ASTKIND_UNKNOWN
	ASTKIND_FBCODE
	ASTKIND_RENAMELIST
	ASTKIND_TITLE

	'' Script helper nodes
	ASTKIND_DECLAREVERSIONS
	ASTKIND_DECLAREBOOL
	ASTKIND_SELECTTARGET
	ASTKIND_SELECTVERSION
	ASTKIND_SELECTDEFINE
	ASTKIND_CASE
	ASTKIND_CASEELSE
	ASTKIND_ENDSELECT
	ASTKIND_OPTION

	'' Declarations/statements
	ASTKIND_PPINCLUDE
	ASTKIND_PPDEFINE
	ASTKIND_PPIF
	ASTKIND_PPELSEIF
	ASTKIND_PPELSE
	ASTKIND_PPENDIF
	ASTKIND_PRAGMAONCE
	ASTKIND_INCLIB
	ASTKIND_UNDEF
	ASTKIND_STRUCT
	ASTKIND_UNION
	ASTKIND_ENUM
	ASTKIND_TYPEDEF
	ASTKIND_CONST
	ASTKIND_VAR
	ASTKIND_FIELD
	ASTKIND_PROC
	ASTKIND_PARAM
	ASTKIND_MACROPARAM
	ASTKIND_ARRAY
	ASTKIND_EXTERNBLOCKBEGIN
	ASTKIND_EXTERNBLOCKEND
	ASTKIND_RETURN
	ASTKIND_ASSIGN
	ASTKIND_SELFOR
	ASTKIND_SELFXOR
	ASTKIND_SELFAND
	ASTKIND_SELFSHL
	ASTKIND_SELFSHR
	ASTKIND_SELFADD
	ASTKIND_SELFSUB
	ASTKIND_SELFMUL
	ASTKIND_SELFDIV
	ASTKIND_SELFMOD
	ASTKIND_IFBLOCK
	ASTKIND_IFPART
	ASTKIND_ELSEIFPART
	ASTKIND_ELSEPART
	ASTKIND_DOWHILE
	ASTKIND_WHILE

	'' Expression atoms etc.
	ASTKIND_CONSTI
	ASTKIND_CONSTF
	ASTKIND_TEXT
	ASTKIND_STRING
	ASTKIND_CHAR
	ASTKIND_DATATYPE
	ASTKIND_ELLIPSIS

	'' Expressions

	'' BOPs
	ASTKIND_CLOGOR
	ASTKIND_CLOGAND
	ASTKIND_LOGOR
	ASTKIND_LOGAND
	ASTKIND_OR
	ASTKIND_XOR
	ASTKIND_AND
	ASTKIND_CCOMMA
	ASTKIND_CASSIGN
	ASTKIND_CSELFOR
	ASTKIND_CSELFXOR
	ASTKIND_CSELFAND
	ASTKIND_CSELFSHL
	ASTKIND_CSELFSHR
	ASTKIND_CSELFADD
	ASTKIND_CSELFSUB
	ASTKIND_CSELFMUL
	ASTKIND_CSELFDIV
	ASTKIND_CSELFMOD
	ASTKIND_CEQ
	ASTKIND_CNE
	ASTKIND_CLT
	ASTKIND_CLE
	ASTKIND_CGT
	ASTKIND_CGE
	ASTKIND_EQ
	ASTKIND_NE
	ASTKIND_LT
	ASTKIND_LE
	ASTKIND_GT
	ASTKIND_GE
	ASTKIND_SHL
	ASTKIND_SHR
	ASTKIND_ADD
	ASTKIND_SUB
	ASTKIND_MUL
	ASTKIND_DIV
	ASTKIND_MOD
	ASTKIND_INDEX
	ASTKIND_MEMBER
	ASTKIND_MEMBERDEREF
	ASTKIND_STRCAT

	'' UOPs
	ASTKIND_CLOGNOT
	ASTKIND_NOT
	ASTKIND_NEGATE
	ASTKIND_UNARYPLUS
	ASTKIND_CDEFINED
	ASTKIND_DEFINED
	ASTKIND_ADDROF
	ASTKIND_DEREF
	ASTKIND_STRINGIFY
	ASTKIND_SIZEOF
	ASTKIND_CAST

	'' Special expressions
	ASTKIND_IIF
	ASTKIND_PPMERGE
	ASTKIND_CALL
	ASTKIND_STRUCTINIT
	ASTKIND_ARRAYINIT
	ASTKIND_DIMENSION

	ASTKIND__COUNT
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

type AstNode as AstNode_

type AstVisitor extends object
	'' result = boolean = whether to visit this node's children
	'' (can be used to skip #define bodies, etc.)
	declare abstract function visit(byref as AstNode) as integer
end type

type ASTVISITCALLBACK as function(byval as AstNode ptr) as integer

'' When changing, adjust clone(), astIsEqual(), dump*()
type AstNode_
	kind		as integer  '' ASTKIND_*
	attrib		as integer  '' ASTATTRIB_*

	'' Identifiers/string literals, or NULL
	text   as zstring ptr '' Symbol name (original or renamed)
	alias_ as zstring ptr '' External name (if symbol was renamed, or if given via asm() in C code, etc.)
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
	as AstNode ptr head, tail, nxt, prev

	declare function getIndexOf(byval lookfor as AstNode ptr) as integer
	declare function hasChild(byval lookfor as AstNode ptr) as integer
	declare sub takeChildren(byval s as AstNode ptr)
	declare sub takeAndPrependChildren(byval s as AstNode ptr)
	declare sub takeAndAppendChildSequence(byval s as AstNode ptr, byval first as AstNode ptr, byval last as AstNode ptr)
	declare function cloneChildren() as AstNode ptr
	declare function groupContains(byval lookfor as AstNode ptr) as integer
	declare function groupContainsAnyChildrenOf(byval other as AstNode ptr) as integer
	declare function groupContainsAllChildrenOf(byval other as AstNode ptr) as integer
	declare function groupsContainEqualChildren(byval other as AstNode ptr) as integer
	declare destructor()
	declare sub insert(byval n as AstNode ptr, byval ref as AstNode ptr)
	declare sub prepend(byval n as AstNode ptr)
	declare sub append(byval n as AstNode ptr)
	declare sub unlink(byval n as AstNode ptr)
	declare function remove(byval n as AstNode ptr) as AstNode ptr
	declare sub removeChildren()
	declare function replace(byval old as AstNode ptr, byval n as AstNode ptr) as AstNode ptr
	declare sub setText(byval newtext as zstring ptr)
	declare sub setAlias(byval newalias as zstring ptr)
	declare sub renameSymbol(byval newid as zstring ptr)
	declare sub renameSymbolWithoutSettingOrigId(byval newid as zstring ptr)
	declare sub takeAliasFromId(byval src as AstNode ptr)
	declare sub takeOrigId(byval src as AstNode ptr)
	declare sub takeAliasAndOrigId(byval src as AstNode ptr)
	declare sub copyOrigId(byval src as AstNode ptr)
	declare sub setType(byval dtype as integer, byval subtype as AstNode ptr)
	declare function cloneNode() as AstNode ptr
	declare function clone() as AstNode ptr
	declare function contains(byval astkind as integer) as integer
	declare function containsCAssignments() as integer
	declare function has1Child() as integer
	declare function hasOnlyChild(byval astkind as integer) as integer
	declare function isCodeBlock() as integer
	declare function isCodeScopeBlock() as integer
	declare function isScopeBlockWith1Stmt() as integer
	declare function isMergableBlock() as integer
	declare function isCastTo(byval dtype as integer, byval subtype as AstNode ptr) as integer
	declare function evalConstiAsInt64() as longint
	declare function isConst0() as integer
	declare function lookupMacroParam(byval id as zstring ptr) as integer
	declare function getMacroParamByNameIgnoreCase(byval id as zstring ptr) as AstNode ptr
	declare sub visit(byref visitor as AstVisitor)
	declare sub visit(byval callback as ASTVISITCALLBACK)
	declare function count() as integer
	declare function dumpPrettyDecl(byval show_type as integer) as string
	declare function dumpOne() as string
	declare sub dump(byval nestlevel as integer = 0, byref prefix as string = "")
end type
#assert sizeof(AstNode) <= sizeof(any ptr) * 15 + sizeof(ApiBits) + 4  '4 bytes padding are added on win32

#define astNewTEXT(text) astNew(ASTKIND_TEXT, text)
#define astNewDEFINED(id) astNew(ASTKIND_DEFINED, id)
#define astIsCONSTI(n) ((n)->kind = ASTKIND_CONSTI)
#define astIsVERBLOCK(n) ((n)->kind = ASTKIND_VERBLOCK)
#define astIsVERAND(n) ((n)->kind = ASTKIND_VERAND)
#define astIsVEROR(n)  ((n)->kind = ASTKIND_VEROR)
#define astIsTEXT(n) ((n)->kind = ASTKIND_TEXT)
#define astIsDEFINED(n) ((n)->kind = ASTKIND_DEFINED)
#define astIsPPIF(n) ((n)->kind = ASTKIND_PPIF)
#define astIsPPELSEIF(n) ((n)->kind = ASTKIND_PPELSEIF)
#define astIsPPELSE(n) ((n)->kind = ASTKIND_PPELSE)
#define astIsPPENDIF(n) ((n)->kind = ASTKIND_PPENDIF)
#define astIsNOT(n) ((n)->kind = ASTKIND_NOT)

declare function astNew overload(byval kind as integer) as AstNode ptr
declare function astNew overload(byval kind as integer, byval text as zstring ptr) as AstNode ptr
declare function astNew overload(byval kind as integer, byval c1 as AstNode ptr, byval c2 as AstNode ptr = NULL) as AstNode ptr
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

declare function astIsEqual _
	( _
		byval a as AstNode ptr, _
		byval b as AstNode ptr, _
		byval is_merge as integer _
	) as integer

declare function astDumpPrettyKind(byval astkind as integer) as string
declare function hGetFbNumberLiteralPrefix(byval attrib as integer) as string
