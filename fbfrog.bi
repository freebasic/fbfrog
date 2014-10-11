const NULL = 0
const FALSE = 0
const TRUE = -1

declare function min( byval a as integer, byval b as integer ) as integer
declare function max( byval a as integer, byval b as integer ) as integer

enum
	CH_BELL      = &h07  '' \a
	CH_BACKSPACE = &h08  '' \b
	CH_TAB       = &h09  '' \t
	CH_LF        = &h0A  '' \n
	CH_VTAB      = &h0B  '' \v
	CH_FORMFEED  = &h0C  '' \f
	CH_CR        = &h0D  '' \r
	CH_ESC       = &h1B

	CH_SPACE     = &h20
	CH_EXCL         '' !
	CH_DQUOTE       '' "
	CH_HASH         '' #
	CH_DOLLAR       '' $
	CH_PERCENT      '' %
	CH_AMP          '' &
	CH_QUOTE        '' '
	CH_LPAREN       '' (
	CH_RPAREN       '' )
	CH_STAR         '' *
	CH_PLUS         '' +
	CH_COMMA        '' ,
	CH_MINUS        '' -
	CH_DOT          '' .
	CH_SLASH        '' /

	CH_0, CH_1, CH_2, CH_3, CH_4, CH_5, CH_6, CH_7, CH_8, CH_9

	CH_COLON        '' :
	CH_SEMI         '' ;
	CH_LT           '' <
	CH_EQ           '' =
	CH_GT           '' >
	CH_QUEST        '' ?
	CH_AT           '' @

	CH_A, CH_B, CH_C, CH_D, CH_E, CH_F, CH_G
	CH_H, CH_I, CH_J, CH_K, CH_L, CH_M, CH_N, CH_O, CH_P
	CH_Q, CH_R, CH_S, CH_T, CH_U, CH_V, CH_W
	CH_X, CH_Y, CH_Z

	CH_LBRACKET     '' [
	CH_BACKSLASH    '' \
	CH_RBRACKET     '' ]
	CH_CIRC         '' ^
	CH_UNDERSCORE   '' _
	CH_GRAVE        '' `

	CH_L_A, CH_L_B, CH_L_C, CH_L_D, CH_L_E, CH_L_F, CH_L_G
	CH_L_H, CH_L_I, CH_L_J, CH_L_K, CH_L_L, CH_L_M, CH_L_N, CH_L_O, CH_L_P
	CH_L_Q, CH_L_R, CH_L_S, CH_L_T, CH_L_U, CH_L_V, CH_L_W
	CH_L_X, CH_L_Y, CH_L_Z

	CH_LBRACE       '' {
	CH_PIPE         '' |
	CH_RBRACE       '' }
	CH_TILDE        '' ~

	CH_DEL
end enum

type SOURCEBUFFER as SOURCEBUFFER_

type TKLOCATION
	'' Supposed to point to permanent memory, whoever fills the structure
	'' 1st must ensure that
	source		as SOURCEBUFFER ptr

	linenum		as integer  '' 0-based, but displayed 1-based in error messages
	column		as integer  '' ditto
	length		as integer
end type

type SOURCEBUFFER_
	is_file		as integer    '' whether name is a file path
	name		as zstring ptr
	location	as TKLOCATION
	buffer		as ubyte ptr  '' file content, null-terminated
	size		as integer    '' allocated buffer size
	lines		as integer    '' number of lines counted during lexing
end type

declare sub sourcebuffersInit( )
declare function hDumpSourceBuffer( byval source as SOURCEBUFFER ptr ) as string
declare function sourcebufferAdd( byval filename as zstring ptr ) as SOURCEBUFFER ptr
declare function sourcebufferFromFile _
	( _
		byval filename as zstring ptr, _
		byval location as TKLOCATION ptr _
	) as SOURCEBUFFER ptr
declare function sourcebufferFromZstring _
	( _
		byval filename as zstring ptr, _
		byval s as zstring ptr, _
		byval location as TKLOCATION ptr _
	) as SOURCEBUFFER ptr
declare sub oops( byval message as zstring ptr )
declare function hDumpLocation( byval location as TKLOCATION ptr ) as string
declare sub hCalcErrorLine _
	( _
		byval column as integer, _
		byval limit as integer, _
		byref s as string, _
		byref offset as integer _
	)
declare function hErrorMarker( byval indent as integer, byval length as integer ) as string
declare function hReport( byval location as TKLOCATION ptr, byval message as zstring ptr ) as string
declare sub oopsLocation( byval location as TKLOCATION ptr, byval message as zstring ptr )
declare function strDuplicate( byval s as zstring ptr ) as zstring ptr
declare function strReplace _
	( _
		byref text as string, _
		byref a as string, _
		byref b as string _
	) as string
declare function strReplaceNonIdChars( byref orig as string, byval replacement as integer ) as string
declare function strMakePrintable( byref a as string ) as string
declare function strIsValidSymbolId( byval s as zstring ptr ) as integer
declare function strMatch( byref s as string, byref pattern as string ) as integer

''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''

'' The hash table is an array of items,
'' which associate a string to some user data.
type THASHITEM
	s	as zstring ptr
	hash	as ulong     '' hash value for quick comparison
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

	'' Whether this hash table should strDuplicate() when storing strings
	'' and free them on hashEnd(). If FALSE, the caller is responsible for
	'' ensuring that strings passed to hashAdd*() stay valid as long as
	'' hashLookup()'s may be done, i.e. typically until hashEnd().
	duplicate_strings	as integer
end type

declare function hashHash( byval s as zstring ptr ) as ulong
declare function hashLookup _
	( _
		byval h as THASH ptr, _
		byval s as zstring ptr, _
		byval hash as ulong _
	) as THASHITEM ptr
declare function hashContains _
	( _
		byval h as THASH ptr, _
		byval s as zstring ptr, _
		byval hash as ulong _
	) as integer
declare sub hashAdd _
	( _
		byval h as THASH ptr, _
		byval item as THASHITEM ptr, _
		byval hash as ulong, _
		byval s as zstring ptr, _
		byval dat as any ptr _
	)
declare function hashAddOverwrite _
	( _
		byval h as THASH ptr, _
		byval s as zstring ptr, _
		byval dat as any ptr _
	) as THASHITEM ptr
declare sub hashInit _
	( _
		byval h as THASH ptr, _
		byval exponent as integer, _
		byval duplicate_strings as integer = FALSE _
	)
declare sub hashEnd( byval h as THASH ptr )
#if __FB_DEBUG__
declare sub hashStats( byval h as THASH ptr, byref prefix as string )
declare sub hashDump( byval h as THASH ptr )
#endif

''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''

extern fbkeywordhash as THASH
declare sub fbkeywordsInit( )

''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''

#if defined( __FB_WIN32__ ) or defined( __FB_DOS__ )
	const PATHDIV = $"\"
#else
	const PATHDIV = "/"
#endif

declare function pathStripExt( byref path as string ) as string
declare function pathExtOnly( byref path as string ) as string
declare function pathOnly( byref path as string ) as string
declare function pathStrip( byref path as string ) as string
declare function pathAddDiv( byref path as string ) as string
declare function pathIsAbsolute( byref s as string ) as integer
declare function pathMakeAbsolute( byref path as string ) as string
declare function hExepath( ) as string
declare function hCurdir( ) as string
declare function pathStripCurdir( byref path as string ) as string
declare function pathIsDir( byref s as string ) as integer
declare function pathNormalize( byref path as string ) as string
declare function hReadableDirExists( byref path as string ) as integer
declare function hFileExists( byref file as string ) as integer

''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''

const TKFLAG_BEHINDSPACE	= 1 shl 0  '' was preceded by spaces?
const TKFLAG_NOEXPAND		= 1 shl 1  '' may be macro-expanded? (cpp)
const TKFLAG_REMOVE		= 1 shl 2
const TKFLAG_D			= 1 shl 3
const TKFLAG_F			= 1 shl 4
const TKFLAG_U			= 1 shl 5
const TKFLAG_L			= 1 shl 6
const TKFLAG_LL			= 1 shl 7
const TKFLAG_HEX		= 1 shl 8
const TKFLAG_OCT		= 1 shl 9
const TKFLAG_FLOAT		= 1 shl 10
const TKFLAG_REMOVEINCLUDE	= 1 shl 11  '' TK_BEGININCLUDE only: Whether the included tokens should be preserved or not

enum
	TK_EOF
	TK_DIVIDER
	TK_BEGIN
	TK_END
	TK_PPMERGE
	TK_ARGBEGIN
	TK_ARGEND
	TK_EOL
	TK_COMMENT
	TK_BEGININCLUDE
	TK_ENDINCLUDE

	'' Number/string literals
	TK_NUMBER
	TK_STRING
	TK_CHAR
	TK_WSTRING
	TK_WCHAR

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

	'' C keywords
	KW__C_FIRST
	KW___ATTRIBUTE__ = KW__C_FIRST
	KW___RESTRICT
	KW___RESTRICT__
	KW__BOOL
	KW__PRAGMA
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
	KW_ERROR
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
	KW_WARNING
	KW_WHILE
	KW__C_LAST = KW_WHILE

	TK_ARGSFILE
	OPT__FIRST
	OPT_NODEFAULTSCRIPT = OPT__FIRST
	OPT_FILTEROUT
	OPT_FILTERIN
	OPT_WHITESPACE
	OPT_WINDOWSMS
	OPT_CONSTANTS
	OPT_NONAMEFIXUP
	OPT_V
	OPT_INCDIR
	OPT_O
	OPT_DECLAREDEFINES
	OPT_UNCHECKED
	OPT_DECLAREVERSIONS
	OPT_DECLAREBOOL
	OPT_SELECT
	OPT_CASE
	OPT_CASEELSE
	OPT_ENDSELECT
	OPT_IFDEF
	OPT_ELSE
	OPT_ENDIF
	OPT_INCLIB
	OPT_DEFINE
	OPT_NOEXPAND
	OPT_REMOVEDEFINE
	OPT_TYPEDEFHINT
	OPT_RESERVEDID
	OPT_RENAMETYPEDEF
	OPT_RENAMETAG
	OPT__LAST = OPT_RENAMETAG

	TK__COUNT
end enum

declare function tkInfoText( byval id as integer ) as zstring ptr
declare function tkInfoPretty( byval tk as integer ) as string

'' Debugging helper, for example: TRACE( x ), "decl begin"
#define TRACE( x ) print __FUNCTION__ + "(" + str( __LINE__ ) + "): " + tkDumpOne( x )

declare sub tkInit( )
declare sub tkDontReportContext( )
declare sub tkEnd( )
declare function hDumpComment( byval comment as zstring ptr ) as string
declare function tkDumpOne( byval x as integer ) as string
declare sub tkDump overload( byval first as integer, byval last as integer )
declare sub tkDump overload( )
declare function tkGetCount( ) as integer
declare sub tkInsert _
	( _
		byval x as integer, _
		byval id as integer, _
		byval text as zstring ptr = NULL _
	)
declare sub tkRemove( byval first as integer, byval last as integer )
declare sub tkCopy( byval x as integer, byval first as integer, byval last as integer )
declare function tkGet( byval x as integer ) as integer
declare function tkGetText( byval x as integer ) as zstring ptr
declare function tkSpellId( byval x as integer ) as zstring ptr
declare sub tkSetLocation( byval x as integer, byval location as TKLOCATION ptr )
declare function tkGetLocation( byval x as integer ) as TKLOCATION ptr
declare sub tkSetExpansionLevel( byval first as integer, byval last as integer, byval expansionlevel as integer )
declare function tkGetExpansionLevel( byval x as integer ) as integer
declare function tkFindTokenWithMinExpansionLevel( byval first as integer, byval last as integer ) as integer
declare function tkGetMaxExpansionLevel( byval first as integer, byval last as integer ) as integer
declare sub tkAddFlags( byval x as integer, byval flags as integer )
declare sub tkSetRemove overload( byval x as integer )
declare sub tkSetRemove overload( byval first as integer, byval last as integer )
declare function tkGetFlags( byval x as integer ) as integer
declare sub tkSetComment( byval x as integer, byval comment as zstring ptr )
declare function tkGetComment( byval x as integer ) as zstring ptr
declare function tkCount _
	( _
		byval tk as integer, _
		byval first as integer, _
		byval last as integer _
	) as integer
declare function tkSkipComment _
	( _
		byval x as integer, _
		byval delta as integer = 1 _
	) as integer
declare function tkSkipCommentEol _
	( _
		byval x as integer, _
		byval delta as integer = 1 _
	) as integer
declare function tkCollectComments _
	( _
		byval first as integer, _
		byval last as integer _
	) as string
declare sub tkApplyRemoves( )
declare sub tkTurnCPPTokensIntoCIds( )
declare function tkSpell overload( byval x as integer ) as string
declare function tkSpell overload( byval first as integer, byval last as integer ) as string
declare function hSkipConstruct( byval x as integer ) as integer
declare function tkReport( byval x as integer, byval message as zstring ptr ) as string
declare sub tkOops( byval x as integer, byval message as zstring ptr )
declare function tkButFound( byval x as integer ) as string
declare function tkMakeExpectedMessage( byval x as integer, byval message as zstring ptr ) as string
declare sub tkOopsExpected( byval x as integer, byval message as zstring ptr )
declare sub tkExpect( byval x as integer, byval tk as integer, byval message as zstring ptr )

''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''

enum
	TYPE_NONE = 0
	TYPE_ANY
	TYPE_BYTE
	TYPE_UBYTE
	TYPE_SHORT
	TYPE_USHORT
	TYPE_LONG
	TYPE_ULONG
	TYPE_CLONG
	TYPE_CULONG
	TYPE_INTEGER
	TYPE_UINTEGER
	TYPE_LONGINT
	TYPE_ULONGINT
	TYPE_SINGLE
	TYPE_DOUBLE
	TYPE_CLONGDOUBLE
	TYPE_UDT
	TYPE_PROC
	TYPE_ZSTRING
	TYPE_WSTRING
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

declare function typeUnsetBaseConst( byval dtype as integer ) as integer

enum
	'' Internal helper nodes
	ASTCLASS_GROUP = 0
	ASTCLASS_VERBLOCK
	ASTCLASS_VEROR
	ASTCLASS_VERAND
	ASTCLASS_DIVIDER
	ASTCLASS_SCOPEBLOCK
	ASTCLASS_UNKNOWN
	ASTCLASS_RENAMELIST

	'' Script helper nodes
	ASTCLASS_DECLAREDEFINES
	ASTCLASS_DECLAREVERSIONS
	ASTCLASS_DECLAREBOOL
	ASTCLASS_SELECT
	ASTCLASS_CASE
	ASTCLASS_CASEELSE
	ASTCLASS_ENDSELECT
	ASTCLASS_FILE
	ASTCLASS_DIR
	ASTCLASS_NOEXPAND
	ASTCLASS_REMOVEDEFINE
	ASTCLASS_TYPEDEFHINT
	ASTCLASS_RESERVEDID
	ASTCLASS_RENAMETYPEDEF
	ASTCLASS_RENAMETAG
	ASTCLASS_FILTEROUT
	ASTCLASS_FILTERIN
	ASTCLASS_INCLIB
	ASTCLASS_PRAGMAONCE

	'' Preprocessor directives
	ASTCLASS_PPINCLUDE
	ASTCLASS_PPDEFINE
	ASTCLASS_PPIF
	ASTCLASS_PPELSEIF
	ASTCLASS_PPELSE
	ASTCLASS_PPENDIF
	ASTCLASS_PPERROR

	'' Declarations/statements
	ASTCLASS_STRUCT
	ASTCLASS_UNION
	ASTCLASS_ENUM
	ASTCLASS_TYPEDEF
	ASTCLASS_CONST
	ASTCLASS_VAR
	ASTCLASS_FIELD
	ASTCLASS_PROC
	ASTCLASS_PARAM
	ASTCLASS_ARRAY
	ASTCLASS_EXTERNBLOCKBEGIN
	ASTCLASS_EXTERNBLOCKEND

	'' Expression atoms etc.
	ASTCLASS_MACROPARAM
	ASTCLASS_CONSTI
	ASTCLASS_CONSTF
	ASTCLASS_ID
	ASTCLASS_TAGID
	ASTCLASS_TEXT
	ASTCLASS_STRING
	ASTCLASS_CHAR
	ASTCLASS_TYPE

	'' Expressions
	'' Generally the AST expressions should represent FB semantics.
	'' The ASTCLASS_BOP_C* ops are used for easier parsing C expressions,
	'' however astOpsC2FB() has to be used afterwards to convert them to FB.

	'' BOPs
	ASTCLASS_CLOGOR
	ASTCLASS_CLOGAND
	ASTCLASS_ORELSE
	ASTCLASS_ANDALSO
	ASTCLASS_OR
	ASTCLASS_XOR
	ASTCLASS_AND
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
	ASTCLASS_SIZEOFTYPE

	ASTCLASS__COUNT
end enum

const ASTATTRIB_LOCAL         = 1 shl 0  '' VAR
const ASTATTRIB_STATIC        = 1 shl 1  '' VAR
const ASTATTRIB_EXTERN        = 1 shl 2  '' VAR
const ASTATTRIB_OCT           = 1 shl 3  '' CONSTI
const ASTATTRIB_HEX           = 1 shl 4  '' CONSTI
const ASTATTRIB_CDECL         = 1 shl 5  '' PROC
const ASTATTRIB_STDCALL       = 1 shl 6  '' PROC
const ASTATTRIB_HIDECALLCONV  = 1 shl 7  '' Whether the calling convention is covered by an Extern block, in which case it doesn't need to be emitted.
const ASTATTRIB_HIDECASEALIAS = 1 shl 8  '' same for the case-preserving ALIAS
const ASTATTRIB_UNCHECKED     = 1 shl 9
const ASTATTRIB_REPORTED      = 1 shl 10 '' Used to mark #defines about which the CPP has already complained, so it can avoid duplicate error messages
const ASTATTRIB_ENUMCONST     = 1 shl 11  '' CONSTI
const ASTATTRIB_NEEDRENAME    = 1 shl 12
const ASTATTRIB_POISONED      = 1 shl 13
const ASTATTRIB_DUMMYDECL     = 1 shl 14
const ASTATTRIB_ONCE          = 1 shl 15  '' Marks #includes as "#include once"
const ASTATTRIB_PACKED        = 1 shl 16  '' __attribute__((packed))
const ASTATTRIB_VARIADIC      = 1 shl 17  '' PPDEFINE/MACROPARAM: variadic macros
const ASTATTRIB_PARENTHESIZEDMACROPARAM = 1 shl 18
const ASTATTRIB_DUMMYID       = 1 shl 19
const ASTATTRIB_DLLIMPORT     = 1 shl 20

'' When changing, adjust astClone(), astIsEqual(), astDump*()
type ASTNODE
	class		as integer  '' ASTCLASS_*
	attrib		as integer  '' ASTATTRIB_*

	'' Identifiers/string literals, or NULL
	text		as zstring ptr
	comment		as zstring ptr
	alias		as zstring ptr  '' Original identifier, if symbol was renamed

	'' Data type (vars, fields, params, function results, expressions)
	dtype		as integer
	subtype		as ASTNODE ptr
	array		as ASTNODE ptr '' ARRAY holding DIMENSIONs, or NULL
	bits		as ASTNODE ptr '' bitfields only

	'' Source location where this declaration/statement was found
	location	as TKLOCATION

	'' PARAM: initializer
	'' VERBLOCK: version expression
	'' IIF: condition expression
	'' DIMENSION: elements expression
	'' PPDEFINE: macro body expression
	'' PROC: procedure body, if any
	expr		as ASTNODE ptr

	union
		paramcount	as integer  '' PPDEFINE: -1 = #define m, 0 = #define m(), 1 = #define m(a), ...
		maxalign	as integer  '' STRUCT/UNION: FIELD=N/#pragma pack(N)
	end union

	'' Linked list of child nodes, operands/fields/parameters/...
	head		as ASTNODE ptr
	tail		as ASTNODE ptr
	next		as ASTNODE ptr
	prev		as ASTNODE ptr
end type

#define astNewID( id ) astNew( ASTCLASS_ID, id )
#define astNewTEXT( text ) astNew( ASTCLASS_TEXT, text )
#define astIsCONSTI( n ) ((n)->class = ASTCLASS_CONSTI)
#define astIsVERBLOCK( n ) ((n)->class = ASTCLASS_VERBLOCK)
#define astIsVERAND( n ) ((n)->class = ASTCLASS_VERAND)
#define astIsVEROR( n )  ((n)->class = ASTCLASS_VEROR)

declare function astNew overload( byval class_ as integer ) as ASTNODE ptr
declare function astNew overload( byval class_ as integer, byval text as zstring ptr ) as ASTNODE ptr
declare function astNew overload( byval class_ as integer, byval c1 as ASTNODE ptr, byval c2 as ASTNODE ptr = NULL ) as ASTNODE ptr
declare function astNewIncludeOnce( byval filename as zstring ptr ) as ASTNODE ptr
declare function astNewPPDEFINE( byval id as zstring ptr ) as ASTNODE ptr
declare function astNewIIF _
	( _
		byval cond as ASTNODE ptr, _
		byval l as ASTNODE ptr, _
		byval r as ASTNODE ptr _
	) as ASTNODE ptr
declare function astNewGROUP overload( ) as ASTNODE ptr
declare function astNewGROUP overload( byval c1 as ASTNODE ptr, byval c2 as ASTNODE ptr = NULL ) as ASTNODE ptr
declare function astNewDEFINED( byval id as zstring ptr ) as ASTNODE ptr
declare function astCloneChildren( byval src as ASTNODE ptr ) as ASTNODE ptr
declare function astGroupContains( byval group as ASTNODE ptr, byval lookfor as ASTNODE ptr ) as integer
declare function astGroupContainsAnyChildrenOf( byval l as ASTNODE ptr, byval r as ASTNODE ptr ) as integer
declare function astGroupContainsAllChildrenOf( byval l as ASTNODE ptr, byval r as ASTNODE ptr ) as integer
declare function astUngroupOne( byval group as ASTNODE ptr ) as ASTNODE ptr
declare function astTakeLoc( byval n as ASTNODE ptr, byval x as integer ) as ASTNODE ptr
declare sub astDelete( byval n as ASTNODE ptr )
declare sub astInsert _
	( _
		byval parent as ASTNODE ptr, _
		byval n as ASTNODE ptr, _
		byval ref as ASTNODE ptr, _
		byval unique as integer = FALSE _
	)
declare sub astPrepend( byval parent as ASTNODE ptr, byval n as ASTNODE ptr )
declare sub astAppend( byval parent as ASTNODE ptr, byval n as ASTNODE ptr )
declare function astRemove( byval parent as ASTNODE ptr, byval a as ASTNODE ptr ) as ASTNODE ptr
declare sub astRemoveChildren( byval parent as ASTNODE ptr )
declare function astReplace _
	( _
		byval parent as ASTNODE ptr, _
		byval old as ASTNODE ptr, _
		byval n as ASTNODE ptr _
	) as ASTNODE ptr
declare sub astSetText( byval n as ASTNODE ptr, byval text as zstring ptr )
declare sub astRenameSymbol( byval n as ASTNODE ptr, byval newid as zstring ptr )
declare sub astSetType _
	( _
		byval n as ASTNODE ptr, _
		byval dtype as integer, _
		byval subtype as ASTNODE ptr _
	)
declare sub astSetComment( byval n as ASTNODE ptr, byval comment as zstring ptr )
declare sub astAddComment( byval n as ASTNODE ptr, byval comment as zstring ptr )
declare function astCloneNode( byval n as ASTNODE ptr ) as ASTNODE ptr
declare function astClone( byval n as ASTNODE ptr ) as ASTNODE ptr
declare function astIsMergableBlock( byval n as ASTNODE ptr ) as integer
declare function astIsEqual _
	( _
		byval a as ASTNODE ptr, _
		byval b as ASTNODE ptr, _
		byval is_merge as integer = FALSE _
	) as integer
declare sub astReport _
	( _
		byval n as ASTNODE ptr, _
		byval message as zstring ptr, _
		byval more_context as integer = TRUE _
	)
declare sub astOops _
	( _
		byval n as ASTNODE ptr, _
		byval message as zstring ptr, _
		byval more_context as integer = TRUE _
	)
declare function astEvalConstiAsInt64( byval n as ASTNODE ptr ) as longint
declare function astCountDecls( byval code as ASTNODE ptr ) as integer
declare function astCountUnknowns( byval code as ASTNODE ptr ) as integer
declare function astDumpPrettyClass( byval astclass as integer ) as string
declare function astDumpPrettyDecl( byval n as ASTNODE ptr ) as string
declare function astDumpOne( byval n as ASTNODE ptr ) as string
declare sub astDump _
	( _
		byval n as ASTNODE ptr, _
		byval nestlevel as integer = 0, _
		byref prefix as string = "" _
	)

declare sub astCleanUpExpressions( byval code as ASTNODE ptr )
declare sub astTurnStructInitIntoArrayInit( byval n as ASTNODE ptr )
declare function astLookupMacroParam _
	( _
		byval macro as ASTNODE ptr, _
		byval id as zstring ptr _
	) as integer
declare sub astMakeProcsDefaultToCdecl( byval n as ASTNODE ptr )
declare sub astAutoExtern _
	( _
		byval ast as ASTNODE ptr, _
		byval use_stdcallms as integer, _
		byval whitespace as integer _
	)
declare sub astSolveOutArrayTypedefs( byval n as ASTNODE ptr, byval ast as ASTNODE ptr )
declare sub astSolveOutProcTypedefs( byval n as ASTNODE ptr, byval ast as ASTNODE ptr )
declare sub astFixArrayParams( byval n as ASTNODE ptr )
declare sub astTurnPlainZstringIntoByte( byval n as ASTNODE ptr )
declare sub astTurnZstringArrayIntoFixedLengthZstring( byval n as ASTNODE ptr )
declare sub astUnscopeDeclsNestedInStructs( byval n as ASTNODE ptr )
declare sub astNameAnonUdtsAfterFirstAliasTypedef( byval n as ASTNODE ptr )
declare sub astAddForwardDeclsForUndeclaredTagIds( byval ast as ASTNODE ptr )
declare sub astRemoveDummyDecls( byval code as ASTNODE ptr )
declare sub astMakeNestedUnnamedStructsFbCompatible( byval n as ASTNODE ptr )
declare sub astRemoveRedundantTypedefs( byval n as ASTNODE ptr, byval ast as ASTNODE ptr )
declare sub astTurnDefinesIntoConstants( byval code as ASTNODE ptr )
declare sub astReplaceSubtypes _
	( _
		byval n as ASTNODE ptr, _
		byval oldclass as integer, _
		byval oldid as zstring ptr, _
		byval newclass as integer, _
		byval newid as zstring ptr _
	)
declare sub astFixIdsInit( )
declare sub astFixIdsAddReservedId( byval id as zstring ptr )
declare sub astFixIds( byval code as ASTNODE ptr )
declare function astUsesDtype( byval n as ASTNODE ptr, byval dtype as integer ) as integer
declare sub astMergeDIVIDERs( byval n as ASTNODE ptr )
declare sub astAutoAddDividers( byval code as ASTNODE ptr )
declare sub astPrependMaybeWithDivider( byval group as ASTNODE ptr, byval n as ASTNODE ptr )

declare function astDumpPrettyVersion( byval n as ASTNODE ptr ) as string
declare function astNewVERAND( byval a as ASTNODE ptr = NULL, byval b as ASTNODE ptr = NULL ) as ASTNODE ptr
declare function astNewVEROR( byval a as ASTNODE ptr = NULL, byval b as ASTNODE ptr = NULL ) as ASTNODE ptr
declare function astWrapFileInVerblock( byval veror as ASTNODE ptr, byval code as ASTNODE ptr ) as ASTNODE ptr
declare function astMergeVerblocks _
	( _
		byval a as ASTNODE ptr, _
		byval b as ASTNODE ptr _
	) as ASTNODE ptr
declare sub astProcessVerblocks( byval code as ASTNODE ptr )

''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''

declare sub lexInit( )
declare function lexIdentifyCKeyword( byval id as zstring ptr ) as integer
declare function lexLoadC _
	( _
		byval x as integer, _
		byval source as SOURCEBUFFER ptr, _
		byval keep_comments as integer _
	) as integer
declare function lexLoadArgs( byval x as integer, byval source as SOURCEBUFFER ptr ) as integer
declare function lexPeekLine _
	( _
		byval source as SOURCEBUFFER ptr, _
		byval targetlinenum as integer _
	) as string

declare function emitType overload _
	( _
		byval dtype as integer, _
		byval subtype as ASTNODE ptr, _
		byval debugdump as integer = FALSE _
	) as string
declare function emitType overload( byval n as ASTNODE ptr ) as string
declare sub emitFile( byref filename as string, byval ast as ASTNODE ptr )

declare function hFindClosingParen( byval x as integer, byval stop_at_cppdirective as integer = TRUE ) as integer
declare function hSkipToEol( byval x as integer ) as integer
declare function hNumberLiteral( byval x as integer, byval is_cpp as integer ) as ASTNODE ptr
extern as integer cprecedence(ASTCLASS_CLOGOR to ASTCLASS_IIF)
declare function hDefineHead( byref x as integer ) as ASTNODE ptr
declare sub cppInit( )
declare sub cppNoExpandSym( byval id as zstring ptr )
declare sub cppRemoveSym( byval id as zstring ptr )
declare sub cppAddFilter( byval filter as ASTNODE ptr )
declare sub cppMain( )
declare sub cInit( )
declare sub cEnd( )
declare sub cAddTypedef( byval id as zstring ptr )
declare sub cPreParse( )
declare function cFile( ) as ASTNODE ptr

''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''

type FROGVERSION
	verand		as ASTNODE ptr
	options		as ASTNODE ptr
end type

namespace frog
	extern as integer verbose, whitespace
	extern incdirs as ASTNODE ptr

	extern as ASTNODE ptr script
	extern as ASTNODE ptr completeverors, fullveror

	extern as FROGVERSION ptr versions
	extern as integer versioncount
end namespace

declare sub frogPrint( byref s as string )
