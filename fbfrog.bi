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

'' When changing, update the table in ast.bas too!
enum
	TK_FILE
	TK_PPINCLUDE
	TK_PPDEFINE
	TK_STRUCT
	TK_PROCDECL
	TK_VARDECL

	TK_TODO         '' TODOs added as fix-me-markers
	TK_BYTE         '' For stray bytes that don't fit in elsewhere
	TK_EOL
	TK_COMMENT      '' /* ... */
	TK_LINECOMMENT  '' // ...

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
	TK_CIRCUMFLEX   '' ^
	TK_CIRCUMFLEXEQ '' ^=
	TK_UNDERSCORE   '' _
	TK_LBRACE       '' {
	TK_PIPE         '' |
	TK_PIPEEQ       '' |=
	TK_PIPEPIPE     '' ||
	TK_RBRACE       '' }
	TK_TILDE        '' ~

	'' >= TK_ID: keywords/identifiers
	TK_ID           '' Identifiers (a-z, A-Z, 0-9, _, $)

	'' C Keywords (some also used by FB)
	KW__C_FIRST
	KW_AUTO = KW__C_FIRST
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

	'' FB only keywords
	'' Here we only need to have FB keywords that we might produce during
	'' the C-to-FB translation.
	'' Note: The lexer doesn't recognize/use these, they're only added
	'' during the translation process.
	KW__FB_FIRST
	KW_ALIAS = KW__FB_FIRST
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

	TK__COUNT
end enum

type ASTINFO
	is_stmtsep	as integer
	text		as zstring ptr
	debug		as zstring ptr
end type

extern as ASTINFO ast_info(0 to (TK__COUNT - 1))

const TYPEMASK_DT    = &b00000000000000000000000000001111  '' 0..15, enough for TYPE_* enum
const TYPEMASK_PTR   = &b00000000000000000000000011110000  '' 0..15, enough for max. 8 PTRs on a type, like FB
const TYPEMASK_REF   = &b00000000000000000000000100000000  '' 0..1, reference or not?
const TYPEMASK_CONST = &b00000000000001111111111000000000  '' 1 bit per PTR + 1 for the REF + 1 for the toplevel

const TYPEPOS_PTR    = 4  '' PTR mask starts at 4th bit
const TYPEPOS_REF    = TYPEPOS_PTR + 4
const TYPEPOS_CONST  = TYPEPOS_REF + 1

const TYPEMAX_PTR = 8

#define typeGetDtAndPtr( dt ) ((dt) and (TYPEMASK_DT or TYPEMASK_PTR))

enum
	TYPE_INT8 = 0
	TYPE_UINT8
	TYPE_INT16
	TYPE_UINT16
	TYPE_INT32
	TYPE_UINT32
	TYPE_INT64
	TYPE_UINT64
	TYPE_UDT
end enum

type ASTNODE
	id		as integer      '' AST_*, TK_*, KW_*
	text		as zstring ptr  '' Identifiers/literals, or NULL

	'' Data type (vars, fields, params, function results)
	dtype		as integer
	subtype		as zstring ptr

	'' Child nodes, if any (fields, params, #define body, #if condition)
	head		as ASTNODE ptr
	tail		as ASTNODE ptr

	'' Linked list of siblings
	next		as ASTNODE ptr
	prev		as ASTNODE ptr
end type

declare sub astDump( byval n as ASTNODE ptr )

declare function strDuplicate( byval s as zstring ptr ) as zstring ptr
declare function astNew _
	( _
		byval id as integer, _
		byval text as zstring ptr = NULL _
	) as ASTNODE ptr
declare sub astDelete( byval n as ASTNODE ptr )
declare function astClone( byval n as ASTNODE ptr ) as ASTNODE ptr
declare function astGet( byval n as ASTNODE ptr ) as integer
declare function astGetText( byval n as ASTNODE ptr ) as zstring ptr
declare function astIsStmtSep( byval n as ASTNODE ptr ) as integer
declare function astIsAtBOL( byval i as ASTNODE ptr ) as integer
declare function astFindLastInLine( byval i as ASTNODE ptr ) as ASTNODE ptr

declare sub astCloneInto _
	( _
		byval parent as ASTNODE ptr, _
		byval head as ASTNODE ptr, _
		byval tail as ASTNODE ptr _
	)
declare sub astInsert _
	( _
		byval parent as ASTNODE ptr, _
		byval n as ASTNODE ptr, _
		byval ref as ASTNODE ptr _
	)
declare sub astAppend( byval parent as ASTNODE ptr, byval n as ASTNODE ptr )
declare function astContains _
	( _
		byval tree as ASTNODE ptr, _
		byval lookfor as ASTNODE ptr _
	) as integer
declare function astRemove _
	( _
		byval parent as ASTNODE ptr, _
		byval n as ASTNODE ptr, _
		byval count as integer = 1 _
	) as ASTNODE ptr
declare function astRemoveUntilBehindEol _
	( _
		byval parent as ASTNODE ptr, _
		byval i as ASTNODE ptr _
	) as ASTNODE ptr

declare function astNewVARDECL _
	( _
		byval id as zstring ptr, _
		byval dtype as integer, _
		byval subtype as zstring ptr _
	) as ASTNODE ptr

''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''

declare function lexLoadFile( byref filename as string ) as ASTNODE ptr
declare sub cPurgeInlineComments( byval block as ASTNODE ptr )
declare sub cParsePpDirectives( byval block as ASTNODE ptr )
declare sub emitWriteFile( byval block as ASTNODE ptr, byref filename as string )
declare sub emitStats( )

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
end type

extern as FROGSTUFF frog

declare sub oops( byref message as string )
