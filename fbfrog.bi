#define NULL 0
#define FALSE 0
#define TRUE (-1)

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

type FSFILE
	pretty		as string  '' Pretty name from command line or #include
	normed		as string  '' Normalized path used in hash table
end type

declare sub fsInit( )
declare sub fsPush( byval context as FSFILE ptr )
declare sub fsPop( )
declare function fsAdd( byref pretty as string ) as FSFILE ptr
declare function fsGetHead( ) as FSFILE ptr
declare sub fsStats( )

''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''

'' When changing, update the table in ast.bas too!
enum
	TK_EOF
	TK_DIVIDER
	TK_AST
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

	'' String literals, sorted to match STRFLAG_*:
	TK_STRING       '' 000    "foo"
	TK_CHAR         '' 001    'c'
	TK_WSTRING      '' 010    L"foo"
	TK_WCHAR        '' 011    L'c'
	TK_ESTRING      '' 100    "\n"
	TK_ECHAR        '' 101    '\n'
	TK_EWSTRING     '' 110    L"\n"
	TK_EWCHAR       '' 111    L'\n'

	'' C tokens
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

	'' C Keywords
	KW_AUTO
	KW_BREAK
	KW_CASE
	KW_CHAR
	KW_CONST
	KW_CONTINUE
	KW_DEFAULT
	KW_DEFINE
	KW_DEFINED
	KW_DO
	KW_DOUBLE
	KW_ELIF
	KW_ELSE
	KW_ENDIF
	KW_ENUM
	KW_EXTERN
	KW_FLOAT
	KW_FOR
	KW_GOTO
	KW_IF
	KW_IFDEF
	KW_IFNDEF
	KW_INCLUDE
	KW_INLINE
	KW_INT
	KW_LONG
	KW_PRAGMA
	KW_REGISTER
	KW_RESTRICT
	KW_RETURN
	KW_SHORT
	KW_SIGNED
	KW_SIZEOF
	KW_STATIC
	KW_STRUCT
	KW_SWITCH
	KW_TYPEDEF
	KW_UNDEF
	KW_UNION
	KW_UNSIGNED
	KW_VOID
	KW_VOLATILE
	KW_WHILE

	TK__COUNT
end enum

declare function tkInfoText( byval tk as integer ) as zstring ptr

'' Debugging helper, for example: TRACE( x ), "decl begin"
#define TRACE( x ) print __FUNCTION__ + "(" + str( __LINE__ ) + "): " + tkDumpOne( x )

type ASTNODE as ASTNODE_

declare function strDuplicate( byval s as zstring ptr ) as zstring ptr
declare sub tkInit( )
declare sub tkEnd( )
declare sub tkStats( )
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
declare function tkGet( byval x as integer ) as integer
declare function tkGetText( byval x as integer ) as zstring ptr
declare function tkGetAst( byval x as integer ) as ASTNODE ptr
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
declare function tkToText( byval first as integer, byval last as integer ) as string
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
	TYPE_LONGINT
	TYPE_ULONGINT
	TYPE_SINGLE
	TYPE_DOUBLE
	TYPE_UDT
	TYPE_PROC
	TYPE__COUNT
end enum

const TYPEMASK_DT    = &b00000000000000000000000000001111  '' 0..15, enough for TYPE_* enum
const TYPEMASK_PTR   = &b00000000000000000000000011110000  '' 0..15, enough for max. 8 PTRs on a type, like FB
const TYPEMASK_REF   = &b00000000000000000000000100000000  '' 0..1, reference or not?
const TYPEMASK_CONST = &b00000000000000111111111000000000  '' 1 bit per PTR + 1 for the toplevel

const TYPEPOS_PTR    = 4  '' PTR mask starts at 4th bit
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

enum
	ASTCLASS_NOP = 0
	ASTCLASS_GROUP
	ASTCLASS_DIVIDER

	ASTCLASS_PPINCLUDE
	ASTCLASS_PPDEFINE
	ASTCLASS_PPIF
	ASTCLASS_PPELSE
	ASTCLASS_PPENDIF
	ASTCLASS_PPUNKNOWN

	ASTCLASS_STRUCT
	ASTCLASS_TYPEDEF
	ASTCLASS_VAR
	ASTCLASS_FIELD
	ASTCLASS_PROC
	ASTCLASS_PARAM
	ASTCLASS_UNKNOWN

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
	ASTATTRIB_EXTERN = &h00000001
	ASTATTRIB_STATIC = &h00000002
end enum

type ASTNODE_
	class		as integer  '' ASTCLASS_*
	attrib		as integer  '' ASTATTRIB_*

	'' Identifiers/string literals, or NULL
	text		as zstring ptr

	comment		as zstring ptr
	intval		as longint

	'' Data type (vars, fields, params, function results, expressions)
	dtype		as integer
	subtype		as ASTNODE ptr

	'' Source location where this declaration/statement was found
	sourcefile	as FSFILE ptr
	sourceline	as integer

	'' Operands (expressions), fields (STRUCT), parameters (PROC)
	head		as ASTNODE ptr
	tail		as ASTNODE ptr
	next		as ASTNODE ptr  '' Siblings in this list
	prev		as ASTNODE ptr
end type

declare function astNew overload( byval class_ as integer ) as ASTNODE ptr
declare function astNew overload _
	( _
		byval class_ as integer, _
		byval a as ASTNODE ptr, _
		byval b as ASTNODE ptr, _
		byval c as ASTNODE ptr _
	) as ASTNODE ptr
declare function astNew overload _
	( _
		byval class_ as integer, _
		byval text as zstring ptr _
	) as ASTNODE ptr
declare sub astDelete( byval n as ASTNODE ptr )
declare sub astAddChild( byval parent as ASTNODE ptr, byval child as ASTNODE ptr )
declare sub astSetText( byval n as ASTNODE ptr, byval text as zstring ptr )
declare sub astSetType _
	( _
		byval n as ASTNODE ptr, _
		byval dtype as integer, _
		byval subtype as ASTNODE ptr _
	)
declare sub astAddComment( byval n as ASTNODE ptr, byval comment as zstring ptr )
declare function astClone( byval n as ASTNODE ptr ) as ASTNODE ptr
declare function astNewCONSTi _
	( _
		byval i as longint, _
		byval dtype as integer _
	) as ASTNODE ptr
declare function astDumpOne( byval n as ASTNODE ptr ) as string
declare sub astDump( byval n as ASTNODE ptr, byval nestlevel as integer = 0 )

''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''

declare function lexLoadFile( byval x as integer, byref filename as string ) as integer
declare function emitType _
	( _
		byval dtype as integer, _
		byval subtype as ASTNODE ptr, _
		byval debugdump as integer = FALSE _
	) as string
declare function emitAst _
	( _
		byval n as ASTNODE ptr, _
		byval need_parens as integer = FALSE _
	) as string
declare sub emitWriteFile( byref filename as string, byref text as string )

declare sub ppComments( )
declare sub ppDividers( )
declare sub ppDirectives1( )
declare sub ppDirectives2( )
declare function cSkip( byval x as integer ) as integer
declare function cSkipRev( byval x as integer ) as integer
declare function cSkipStatement( byval x as integer ) as integer
declare function cToplevel( ) as ASTNODE ptr

''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''

type DEPNODE
	f		as FSFILE ptr

	children	as DEPNODE ptr ptr
	childcount	as integer
	childroom	as integer

	missing		as integer
end type

declare sub depInit( )
declare function depLookup( byval f as FSFILE ptr ) as DEPNODE ptr
declare function depAdd( byref pretty as string ) as DEPNODE ptr
declare sub depOn( byval node as DEPNODE ptr, byval child as DEPNODE ptr )
declare sub depScan( )
declare sub depPrintFlat( )

''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''

type FROGSTUFF
	dep		as integer
	merge		as integer
	verbose		as integer
	preset		as string
end type

extern as FROGSTUFF frog

declare sub oops( byref message as string )
declare function strReplace _
	( _
		byref text as string, _
		byref a as string, _
		byref b as string _
	) as string
