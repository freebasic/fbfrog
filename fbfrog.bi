#define NULL 0
#define FALSE 0
#define TRUE (-1)

declare sub oops( byref message as string )
declare function strDuplicate( byval s as zstring ptr ) as zstring ptr
declare function strReplace _
	( _
		byref text as string, _
		byref a as string, _
		byref b as string _
	) as string
declare function strMatches _
	( _
		byref origpattern as string, _
		byref s as string _
	) as integer
declare function strStartsWith( byref s as string, byref lookfor as string ) as integer
declare function strEndsWith( byref s as string, byref lookfor as string ) as integer

''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''

'' The hash table is an array of items,
'' which associate a string to some user data.
type THASHITEM
	s	as zstring ptr
	hash	as uinteger  '' hash value for quick comparison
	data	as any ptr   '' user data
end type

type THASH
	items		as THASHITEM ptr
	count		as integer  '' number of used items
	room		as integer  '' number of allocated items
	resizes		as integer  '' number of table reallocs/size increases
	lookups		as integer  '' lookup counter
	perfects	as integer  '' lookups successful after first probe
	collisions	as integer  '' sum of collisions during all lookups
end type

declare function hashHash( byval s as zstring ptr ) as uinteger
declare function hashLookup _
	( _
		byval h as THASH ptr, _
		byval s as zstring ptr, _
		byval hash as uinteger _
	) as THASHITEM ptr
declare sub hashAdd _
	( _
		byval h as THASH ptr, _
		byval item as THASHITEM ptr, _
		byval hash as uinteger, _
		byval s as zstring ptr, _
		byval dat as any ptr _
	)
declare sub hashAddOverwrite _
	( _
		byval h as THASH ptr, _
		byval s as zstring ptr, _
		byval dat as any ptr _
	)
declare sub hashInit( byval h as THASH ptr, byval exponent as integer )
declare sub hashEnd( byval h as THASH ptr )
declare sub hashStats( byval h as THASH ptr, byref prefix as string )

''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''

type LISTNODE
	next		as LISTNODE ptr
	prev		as LISTNODE ptr
end type

type TLIST
	head		as LISTNODE ptr
	tail		as LISTNODE ptr
	nodesize	as integer
end type

declare function listGetHead( byval l as TLIST ptr) as any ptr
declare function listGetTail( byval l as TLIST ptr) as any ptr
declare function listGetNext( byval p as any ptr ) as any ptr
declare function listGetPrev( byval p as any ptr ) as any ptr
declare function listAppend( byval l as TLIST ptr ) as any ptr
declare sub listDelete( byval l as TLIST ptr, byval p as any ptr )
declare function listCount( byval l as TLIST ptr ) as integer
declare sub listInit( byval l as TLIST ptr, byval unit as integer )
declare sub listEnd( byval l as TLIST ptr )

''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''

declare function pathStripExt( byref path as string ) as string
declare function pathExtOnly( byref path as string ) as string
declare function pathOnly( byref path as string ) as string
declare function pathAddDiv( byref path as string ) as string
declare function pathStripLastComponent( byref path as string ) as string
declare function pathFindCommonBase _
	( _
		byref aorig as string, _
		byref borig as string _
	) as string
declare function pathStripCommonBase _
	( _
		byref a as string, _
		byref b as string _
	) as string
declare function pathMakeAbsolute( byref path as string ) as string
declare function pathNormalize( byref path as string ) as string
declare function hFileExists( byref file as string ) as integer
declare sub hScanDirectoryForH _
	( _
		byref rootdir as string, _
		byval resultlist as TLIST ptr _
	)

''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''

'' When changing, update the table in ast.bas too!
enum
	TK_EOF
	TK_DIVIDER
	TK_PPINCLUDE
	TK_PPDEFINE
	TK_PPIF
	TK_PPELSEIF
	TK_PPELSE
	TK_PPENDIF
	TK_PPUNDEF
	TK_PPUNKNOWN
	TK_BEGIN
	TK_END

	TK_BYTE         '' For stray bytes that don't fit in elsewhere
	TK_SPACE        '' Spaces/tabs (merged)
	TK_EOL
	TK_COMMENT

	'' Number literals
	TK_DECNUM
	TK_HEXNUM
	TK_OCTNUM
	TK_DECFLOAT

	'' String literals, sorted to match STRFLAG_*:
	TK_STRING       '' 000    "foo"
	TK_CHAR         '' 001    'c'
	TK_WSTRING      '' 010    L"foo"
	TK_WCHAR        '' 011    L'c'
	TK_ESTRING      '' 100    "\n"
	TK_ECHAR        '' 101    '\n'
	TK_EWSTRING     '' 110    L"\n"
	TK_EWCHAR       '' 111    L'\n'

	'' C/FB tokens
	TK_EXCL         '' !
	TK_EXCLEQ       '' !=
	TK_HASH         '' #
	TK_HASHHASH     '' ##
	TK_PERCENT      '' %
	TK_PERCENTEQ    '' %=
	TK_AMP          '' &
	TK_AMPEQ        '' &=
	TK_AMPAMP       '' &&
	TK_LPAREN       '' (
	TK_RPAREN       '' )
	TK_STAR         '' *
	TK_STAREQ       '' *=
	TK_PLUS         '' +
	TK_PLUSEQ       '' +=
	TK_PLUSPLUS     '' ++
	TK_COMMA        '' ,
	TK_MINUS        '' -
	TK_MINUSEQ      '' -=
	TK_MINUSMINUS   '' --
	TK_ARROW        '' ->
	TK_DOT          '' .
	TK_ELLIPSIS     '' ...
	TK_SLASH        '' /
	TK_SLASHEQ      '' /=
	TK_COLON        '' :
	TK_SEMI         '' ;
	TK_LT           '' <
	TK_LTLT         '' <<
	TK_LTLTEQ       '' <<=
	TK_LTEQ         '' <=
	TK_LTGT         '' <>
	TK_EQ           '' =
	TK_EQEQ         '' ==
	TK_GT           '' >
	TK_GTGT         '' >>
	TK_GTGTEQ       '' >>=
	TK_GTEQ         '' >=
	TK_QUEST        '' ?
	TK_AT           '' @
	TK_LBRACKET     '' [
	TK_BACKSLASH    '' \
	TK_RBRACKET     '' ]
	TK_CIRC         '' ^
	TK_CIRCEQ       '' ^=
	TK_UNDERSCORE   '' _
	TK_LBRACE       '' {
	TK_PIPE         '' |
	TK_PIPEEQ       '' |=
	TK_PIPEPIPE     '' ||
	TK_RBRACE       '' }
	TK_TILDE        '' ~

	'' >= TK_ID: keywords/identifiers
	TK_ID           '' Identifiers (a-z, A-Z, 0-9, _, $)

	'' C-only keywords
	KW__C_FIRST
	KW___ATTRIBUTE__ = KW__C_FIRST
	KW___CDECL
	KW___RESTRICT
	KW___RESTRICT__
	KW___STDCALL
	KW_AUTO
	KW_BREAK
	KW_CHAR
	KW_DEFAULT
	KW_ELIF
	KW_FLOAT
	KW_INLINE
	KW_REGISTER
	KW_RESTRICT
	KW_SIGNED
	KW_STRUCT
	KW_SWITCH
	KW_TYPEDEF
	KW_VOID
	KW_VOLATILE

	'' C/FB shared keywords
	KW__FB_FIRST
	KW_CASE = KW__FB_FIRST
	KW_CONST
	KW_CONTINUE
	KW_DEFINE
	KW_DEFINED
	KW_DO
	KW_DOUBLE
	KW_ELSE
	KW_ENDIF
	KW_ENUM
	KW_EXTERN
	KW_FOR
	KW_GOTO
	KW_IF
	KW_IFDEF
	KW_IFNDEF
	KW_INCLUDE
	KW_INT
	KW_LONG
	KW_PRAGMA
	KW_RETURN
	KW_SHORT
	KW_SIZEOF
	KW_STATIC
	KW_UNDEF
	KW_UNION
	KW_UNSIGNED
	KW_WHILE
	KW__C_LAST = KW_WHILE

	'' FB-only keywords
	KW_ALIAS
	KW_AND
	KW_ANDALSO
	KW_ANY
	KW_AS
	KW_BYTE
	KW_BYVAL
	KW_CAST
	KW_CDECL
	KW_CPTR
	KW_DECLARE
	KW_DIM
	KW_ELSEIF
	KW_END
	KW_EXIT
	KW_EXPORT
	KW_FIELD
	KW_FUNCTION
	KW_IIF
	KW_INTEGER
	KW_LONGINT
	KW_LOOP
	KW_MOD
	KW_NEXT
	KW_NOT
	KW_OR
	KW_ORELSE
	KW_PASCAL
	KW_PRIVATE
	KW_PTR
	KW_SCOPE
	KW_SELECT
	KW_SHARED
	KW_SHL
	KW_SHR
	KW_SINGLE
	KW_STDCALL
	KW_SUB
	KW_THEN
	KW_TO
	KW_TYPE
	KW_TYPEOF
	KW_UBYTE
	KW_UINTEGER
	KW_ULONG
	KW_ULONGINT
	KW_USHORT
	KW_WEND
	KW_WSTR
	KW_WSTRING
	KW_XOR
	KW_ZSTRING
	KW__FB_LAST = KW_ZSTRING

	TK__COUNT
end enum

declare function tkInfoText( byval tk as integer ) as zstring ptr

'' Debugging helper, for example: TRACE( x ), "decl begin"
#define TRACE( x ) print __FUNCTION__ + "(" + str( __LINE__ ) + "): " + tkDumpOne( x )

type ASTNODE as ASTNODE_
type FROGFILE as FROGFILE_

declare sub tkInit( )
declare sub tkEnd( )
declare function tkDumpBasic( byval id as integer, byval text as zstring ptr ) as string
declare function tkDumpOne( byval x as integer ) as string
declare sub tkDump( )
declare function tkGetCount( ) as integer
declare sub tkInsert _
	( _
		byval x as integer, _
		byval id as integer, _
		byval text as zstring ptr = NULL, _
		byval ast as ASTNODE ptr = NULL _
	)
declare sub tkRemove( byval first as integer, byval last as integer )
declare sub tkCopy( byval x as integer, byval first as integer, byval last as integer )
declare function tkGet( byval x as integer ) as integer
declare function tkGetText( byval x as integer ) as zstring ptr
declare function tkGetIdOrKw( byval x as integer ) as zstring ptr
declare function tkGetAst( byval x as integer ) as ASTNODE ptr
declare sub tkSetAst( byval x as integer, byval ast as ASTNODE ptr )
declare sub tkSetPoisoned( byval first as integer, byval last as integer )
declare function tkIsPoisoned( byval x as integer ) as integer
declare sub tkSetLineNum( byval x as integer, byval linenum as integer )
declare function tkGetLineNum( byval x as integer ) as integer
declare sub tkSetComment( byval x as integer, byval comment as zstring ptr )
declare function tkGetComment( byval x as integer ) as zstring ptr
declare function tkCount _
	( _
		byval tk as integer, _
		byval first as integer, _
		byval last as integer _
	) as integer
declare function tkSkipSpaceAndComments _
	( _
		byval x as integer, _
		byval delta as integer = 1 _
	) as integer
declare function tkToCText( byval id as integer, byval text as zstring ptr ) as string
declare function tkManyToCText _
	( _
		byval first as integer, _
		byval last as integer _
	) as string
declare function tkToAstText _
	( _
		byval first as integer, _
		byval last as integer _
	) as ASTNODE ptr
declare function tkCollectComments _
	( _
		byval first as integer, _
		byval last as integer _
	) as string
declare sub tkRemoveAllOf( byval id as integer, byval text as zstring ptr )

''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''

enum
	TYPE_NONE = 0
	TYPE_ANY
	TYPE_BYTE
	TYPE_UBYTE
	TYPE_ZSTRING
	TYPE_SHORT
	TYPE_USHORT
	TYPE_LONG
	TYPE_ULONG
	TYPE_INTEGER
	TYPE_UINTEGER
	TYPE_LONGINT
	TYPE_ULONGINT
	TYPE_SINGLE
	TYPE_DOUBLE
	TYPE_UDT
	TYPE_PROC
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

#define typeSetDt( dtype, dt ) ((dtype and (not TYPEMASK_DT)) or (dt and TYPEMASK_DT))
#define typeSetIsConst( dt ) ((dt) or (1 shl TYPEPOS_CONST))
#define typeIsConstAt( dt, at ) (((dt) and (1 shl (TYPEPOS_CONST + (at)))) <> 0)
#define typeGetDt( dt ) ((dt) and TYPEMASK_DT)
#define typeGetDtAndPtr( dt ) ((dt) and (TYPEMASK_DT or TYPEMASK_PTR))
#define typeGetPtrCount( dt ) (((dt) and TYPEMASK_PTR) shr TYPEPOS_PTR)
#define typeAddrOf( dt ) _
	(((dt) and TYPEMASK_DT) or _
	 (((dt) and TYPEMASK_PTR) + (1 shl TYPEPOS_PTR)) or _
	 (((dt) and TYPEMASK_CONST) shl 1))
#define typeMultAddrOf( dt, count ) _
	((dt and TYPEMASK_DT) or _
	 ((dt and TYPEMASK_PTR) + (count shl TYPEPOS_PTR)) or _
	 ((dt and TYPEMASK_CONST) shl count))
#define typeGetConst( dt ) ((dt) and TYPEMASK_CONST)

declare function typeToSigned( byval dtype as integer ) as integer
declare function typeToUnsigned( byval dtype as integer ) as integer
declare function typeIsFloat( byval dtype as integer ) as integer

enum
	ASTCLASS_NOP = 0
	ASTCLASS_GROUP
	ASTCLASS_VERSION
	ASTCLASS_DIVIDER

	ASTCLASS_PPINCLUDE
	ASTCLASS_PPDEFINE
	ASTCLASS_PPIF
	ASTCLASS_PPELSEIF
	ASTCLASS_PPELSE
	ASTCLASS_PPENDIF
	ASTCLASS_PPUNDEF
	ASTCLASS_PPUNKNOWN

	ASTCLASS_STRUCT
	ASTCLASS_UNION
	ASTCLASS_ENUM
	ASTCLASS_TYPEDEF
	ASTCLASS_STRUCTFWD
	ASTCLASS_VAR
	ASTCLASS_FIELD
	ASTCLASS_ENUMCONST
	ASTCLASS_PROC
	ASTCLASS_PARAM
	ASTCLASS_ARRAY
	ASTCLASS_DIMENSION
	ASTCLASS_UNKNOWN

	ASTCLASS_MACROBODY
	ASTCLASS_MACROPARAM
	ASTCLASS_TK
	ASTCLASS_CONST
	ASTCLASS_ID
	ASTCLASS_TEXT
	ASTCLASS_DEFINED

	ASTCLASS_IIF
	ASTCLASS_LOGOR
	ASTCLASS_LOGAND
	ASTCLASS_BITOR
	ASTCLASS_BITXOR
	ASTCLASS_BITAND
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
	ASTCLASS_LOGNOT
	ASTCLASS_BITNOT
	ASTCLASS_NEGATE
	ASTCLASS_UNARYPLUS

	ASTCLASS__COUNT
end enum

enum
	ASTATTRIB_EXTERN	= 1 shl 0  '' VAR
	ASTATTRIB_PRIVATE	= 1 shl 1  '' VAR
	ASTATTRIB_OCT		= 1 shl 2  '' CONST
	ASTATTRIB_HEX		= 1 shl 3  '' CONST
	ASTATTRIB_PPINDENTBEGIN	= 1 shl 4  '' PP*
	ASTATTRIB_PPINDENTEND	= 1 shl 5  '' PP*
	ASTATTRIB_MERGEWITHPREV	= 1 shl 6  '' TK, MACROPARAM: Macro body tokens that were preceded by the '##' PP merge operator
	ASTATTRIB_STRINGIFY	= 1 shl 7  '' MACROPARAM, to distinguish "param" from "#param"
	ASTATTRIB_CDECL		= 1 shl 8
	ASTATTRIB_STDCALL	= 1 shl 9
end enum

type ASTNODECONST
	union
		i	as longint
		f	as double
	end union
end type

'' When changing, adjust astClone()
type ASTNODE_
	class		as integer  '' ASTCLASS_*
	attrib		as integer  '' ASTATTRIB_*

	'' Identifiers/string literals, or NULL
	text		as zstring ptr
	comment		as zstring ptr

	'' Data type (vars, fields, params, function results, expressions)
	dtype		as integer
	subtype		as ASTNODE ptr
	array		as ASTNODE ptr '' ARRAY holding DIMENSIONs, or NULL

	'' PARAM: initializer
	'' PPDEFINE: macro body
	'' VERSION: version information (a GROUP holding a list of CONSTs)
	initializer	as ASTNODE ptr

	'' Source location where this declaration/statement was found
	sourcefile	as FROGFILE ptr
	sourceline	as integer

	'' PPINCLUDE: back link after the #include was resolved to a real file
	includefile	as FROGFILE ptr

	val		as ASTNODECONST
	tk		as integer  '' ASTCLASS_TK
	paramindex	as integer  '' ASTCLASS_MACROPARAM
	paramcount	as integer  '' ASTCLASS_PPDEFINE: -1 = #define m, 0 = #define m(), 1 = #define m(a), ...

	'' Child nodes: operands/fields/parameters/...
	head		as ASTNODE ptr
	tail		as ASTNODE ptr

	'' Sibling nodes
	next		as ASTNODE ptr
	prev		as ASTNODE ptr
end type

declare function astNew overload( byval class_ as integer ) as ASTNODE ptr
declare function astNew overload _
	( _
		byval class_ as integer, _
		byval a as ASTNODE ptr, _
		byval b as ASTNODE ptr = NULL, _
		byval c as ASTNODE ptr = NULL _
	) as ASTNODE ptr
declare function astNew overload _
	( _
		byval class_ as integer, _
		byval text as zstring ptr _
	) as ASTNODE ptr
declare function astNewVERSION overload _
	( _
		byval child as ASTNODE ptr, _
		byval versionnum as integer _
	) as ASTNODE ptr
declare function astNewVERSION overload _
	( _
		byval child as ASTNODE ptr, _
		byval version1 as ASTNODE ptr, _
		byval version2 as ASTNODE ptr _
	) as ASTNODE ptr
declare function astNewCONST _
	( _
		byval i as longint, _
		byval f as double, _
		byval dtype as integer _
	) as ASTNODE ptr
#define astNewID( id ) astNew( ASTCLASS_ID, id )
declare function astNewTK _
	( _
		byval tk as integer, _
		byval text as zstring ptr _
	) as ASTNODE ptr
declare function astNewMACROPARAM( byval paramindex as integer ) as ASTNODE ptr
declare sub astDelete( byval n as ASTNODE ptr )
declare sub astAddChild( byval parent as ASTNODE ptr, byval n as ASTNODE ptr )
declare sub astCloneAndAddAllChildrenOf( byval d as ASTNODE ptr, byval s as ASTNODE ptr )
declare sub astAddVersionedChild( byval n as ASTNODE ptr, byval child as ASTNODE ptr )
declare function astIsChildOf _
	( _
		byval parent as ASTNODE ptr, _
		byval lookfor as ASTNODE ptr _
	) as integer
declare sub astAddChildBefore _
	( _
		byval parent as ASTNODE ptr, _
		byval n as ASTNODE ptr, _
		byval ref as ASTNODE ptr _
	)
declare function astReplaceChild _
	( _
		byval parent as ASTNODE ptr, _
		byval a as ASTNODE ptr, _
		byval b as ASTNODE ptr _
	) as ASTNODE ptr
declare sub astRemoveChild( byval parent as ASTNODE ptr, byval a as ASTNODE ptr )
declare sub astSetText( byval n as ASTNODE ptr, byval text as zstring ptr )
declare sub astSetType _
	( _
		byval n as ASTNODE ptr, _
		byval dtype as integer, _
		byval subtype as ASTNODE ptr _
	)
declare sub astAddComment( byval n as ASTNODE ptr, byval comment as zstring ptr )
declare function astCloneNode( byval n as ASTNODE ptr ) as ASTNODE ptr
declare function astClone( byval n as ASTNODE ptr ) as ASTNODE ptr
declare function astIsEqualDecl _
	( _
		byval a as ASTNODE ptr, _
		byval b as ASTNODE ptr, _
		byval ignore_fields as integer = FALSE _
	) as integer
declare function astDumpOne( byval n as ASTNODE ptr ) as string
declare function astDumpInline( byval n as ASTNODE ptr ) as string
declare sub astDump( byval n as ASTNODE ptr, byval nestlevel as integer = 0 )

''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''

declare function lexLoadFile _
	( _
		byval x as integer, _
		byref filename as string, _
		byval fb_mode as integer = FALSE _
	) as integer
declare function emitType _
	( _
		byval dtype as integer, _
		byval subtype as ASTNODE ptr, _
		byval debugdump as integer = FALSE _
	) as string
declare sub emitFile( byref filename as string, byval ast as ASTNODE ptr )
declare function importFile( byref file as string ) as ASTNODE ptr

declare sub ppComments( )
declare sub ppDividers( )
declare function hNumberLiteral( byval x as integer ) as ASTNODE ptr
declare sub ppDirectives1( )
declare sub ppDirectives2( )
declare sub ppEvalInit( )
declare sub ppMacroBegin( byval id as zstring ptr, byval paramcount as integer )
declare sub ppMacroToken( byval tk as integer, byval text as zstring ptr = NULL )
declare sub ppMacroParam( byval index as integer )
declare sub ppAddSym( byval id as zstring ptr, byval is_defined as integer )
declare sub ppExpandSym( byval id as zstring ptr )
declare sub ppEval( )
declare sub ppNoEval( )
declare function cSkip( byval x as integer ) as integer
declare function cSkipRev( byval x as integer ) as integer
declare function cSkipStatement _
	( _
		byval x as integer, _
		byval is_enum as integer = FALSE _
	) as integer
declare function cToplevel( ) as ASTNODE ptr

''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''

type FROGFILE_
	pretty		as string '' Pretty name from command line or #include
	normed		as string '' Normalized path used in hash table
	missing		as integer '' File missing/not found?
	ast		as ASTNODE ptr  '' AST representing file content, when loaded
	refcount	as integer
end type

type FROGSTUFF
	merge		as integer
	verbose		as integer
	preset		as string

	files		as TLIST '' FROGFILE
	filehash	as THASH
	commonparent	as string
end type

extern as FROGSTUFF frog
