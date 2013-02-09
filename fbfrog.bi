#define NULL 0
#define FALSE 0
#define TRUE (-1)

''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''

'' The hash table is an array of HASHITEMs, which associate a string to
'' some user data.
type HASHITEM
	s	as zstring ptr
	length	as integer
	hash	as uinteger  '' hash value for quick comparison
	data	as any ptr   '' user data
end type

type THASH
	items		as HASHITEM ptr
	count		as integer  '' number of used items
	room		as integer  '' number of allocated items
	resizes		as integer  '' number of table reallocs/size increases
	lookups		as integer  '' lookup counter
	perfects	as integer  '' lookups successful after first probe
	collisions	as integer  '' sum of collisions during all lookups
end type

declare function hashHash _
	( _
		byval s as ubyte ptr, _
		byval length as integer _
	) as uinteger
declare function hashLookup _
	( _
		byval h as THASH ptr, _
		byval s as ubyte ptr, _
		byval length as integer, _
		byval hash as uinteger _
	) as HASHITEM ptr
declare sub hashAdd _
	( _
		byval h as THASH ptr, _
		byval item as HASHITEM ptr, _
		byval hash as uinteger, _
		byval s as ubyte ptr, _
		byval length as integer, _
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
declare function listGetNext( byval p as any ptr ) as any ptr
declare function listAppend( byval l as TLIST ptr ) as any ptr
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

'' When changing, update the table in tk.bas too!
enum
	TK_EOF = 0
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

'' Note: When updating, update the debugging code too!
enum
	MARK_TOPLEVEL = 0
	MARK_PP        '' Some PP directive
	MARK_PPEXPR    '' #if expression (but not the #if itself)
	MARK_EXTERN    '' EXTERN "C" block
	MARK_ENDEXTERN '' To identify '}'
	MARK_STRUCT    '' [typedef to] struct/union/enum block
	MARK_ENDENUM   '' To identify '}'
	MARK_ENDSTRUCT '' To identify '}'
	MARK_ENDUNION  '' To identify '}'
	MARK_ENUMCONST '' Enum constants
	MARK_TYPEDEF   '' Typedefs (though not those with struct/etc blocks)
	MARK_TOPDECL   '' Toplevel declarations, after parsing, before split up into vardecls/procdecls
	MARK_PROCDECL  '' procdecls after being split from topdecls
	MARK_VARDECL   '' vardecls after being split from topdecls
	MARK_FIELDDECL '' Fields
	MARK_UNKNOWN   '' Unrecognized constructs (skip until ';')
	MARK_UNKNOWNENUMCONST '' Unrecognized constructs inside enums (skip until ',' or '}' instead of ';')
	MARK__COUNT
end enum

extern as zstring ptr token_text(0 to (TK__COUNT - 1))
extern as zstring ptr mark_text(0 to (MARK__COUNT - 1))

'' Great debugging helper, for example: TRACE(x), "decl begin"
#define TRACE( x ) print __FUNCTION__ + "(" + str( __LINE__ ) + "):" + _
	str( x ) + " " + *mark_text(tkMark( x )) + "[" + *token_text(tkGet( x )) + "]"

declare function strDuplicate _
	( _
		byval s as ubyte ptr, _
		byval length as integer _
	) as zstring ptr
declare sub tkRawMoveTo( byval x as integer )
declare sub tkRawInsert( byval id as integer, byval text as ubyte ptr )
declare sub tkInsert _
	( _
		byval x as integer, _
		byval id as integer, _
		byval text as zstring ptr _
	)
declare sub tkCopy _
	( _
		byval x as integer, _
		byval first as integer, _
		byval last as integer _
	)
declare sub tkRemove( byval first as integer, byval last as integer )
declare sub tkSetMark _
	( _
		byval mark as integer, _
		byval first as integer, _
		byval last as integer _
	)
declare function tkGet( byval x as integer ) as integer
declare function tkText( byval x as integer ) as zstring ptr
declare function tkMark( byval x as integer ) as integer
declare function tkCount( ) as integer
declare sub tkInit( )
declare sub tkEnd( )

declare sub emitWriteFile( byref filename as string )
declare sub emitStats( )

declare function lexInsertFile _
	( _
		byval x as integer, _
		byref filename as string _
	) as integer

declare sub preparseToplevel( )
declare sub parseToplevel( byval begin as integer )
declare sub translateToplevel( )

declare function storageStore _
	( _
		byval text as ubyte ptr, _
		byval length as integer, _
		byval pdat as integer ptr _
	) as ubyte ptr
declare sub storageInit( )
declare sub storageStats( )

''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''

enum
	'' Translated already? (no matter which method)
	FILE_VISITED = (1 shl 0)

	'' Found during preparse? (if not, it's from the command line)
	FILE_EXTRA = (1 shl 1)
end enum

type FROGFILE
	softname	as zstring ptr  '' Pretty name from command line or #include
	hardname	as zstring ptr  '' Normalized path used in hash table

	'' This tells how many #includes of this file were found during the
	'' preparse (no matter which parent file).
	'' refcount = 1 means it can be trivially merged into its one parent.
	'' refcount = 0 means it's a "toplevel" file that has no parents (ohh).
	'' Note: This refcount is only valid if the preparse is done...
	refcount	as integer

	flags		as uinteger
end type

enum
	DEFINE_EMPTY = (1 shl 0)
	DEFINE_CALL  = (1 shl 1)
end enum

type FROGSTUFF
	concat		as integer
	follow		as integer
	merge		as integer
	verbose		as integer
	f		as FROGFILE ptr

	files		as TLIST '' FROGFILE
	filehash	as THASH
	definehash	as THASH
end type

extern as FROGSTUFF frog

declare sub oops( byref message as string )
declare function frogAddDefine _
	( _
		byval id as zstring ptr, _
		byval flags as uinteger _
	) as uinteger
declare function frogAddFile _
	( _
		byref origname as string, _
		byval is_preparse as integer, _
		byval search_paths as integer _
	) as FROGFILE ptr
declare sub frogSetVisited( byval f as FROGFILE ptr )
declare function frogCanMerge( byval f as FROGFILE ptr ) as integer
