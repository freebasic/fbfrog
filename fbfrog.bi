#undef FALSE
#undef TRUE
const NULL = 0
const FALSE = 0
const TRUE = -1

declare function min(byval a as integer, byval b as integer) as integer
declare function max(byval a as integer, byval b as integer) as integer

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

type SourceInfo
	name as zstring ptr
	is_file as integer '' whether name is a file path
end type

declare function sourceinfoForZstring(byval prettyname as zstring ptr) byref as SourceInfo

type TkLocation
	source as SourceInfo ptr
	linenum as integer
end type

type FileBuffer
	buffer as zstring ptr '' file content, null-terminated (embedded nulls are disallowed)
	source as SourceInfo
	declare sub load(byval location as TkLocation)
end type

declare function filebuffersAdd(byval filename as zstring ptr, byval location as TkLocation) as FileBuffer ptr

declare sub oops(byval message as zstring ptr)
declare function hDumpLocation(byval location as TkLocation) as string
declare sub hCalcErrorLine _
	( _
		byval column as integer, _
		byval limit as integer, _
		byref s as string, _
		byref offset as integer _
	)
declare function hErrorMarker(byval indent as integer, byval length as integer) as string
declare function hReport(byval location as TkLocation, byval message as zstring ptr) as string
declare sub oopsLocation(byval location as TkLocation, byval message as zstring ptr)
declare function hTrim(byref s as string) as string
declare function hLTrim(byref s as string) as string
declare function strStartsWith(byref s as string, byref lookfor as string) as integer
declare function strDuplicate(byval s as zstring ptr) as zstring ptr
declare sub strSplit(byref s as string, byref delimiter as string, byref l as string, byref r as string)
declare function strReplace _
	( _
		byref text as string, _
		byref a as string, _
		byref b as string _
	) as string
declare function strReplaceNonIdChars(byref orig as string, byval replacement as integer) as string
declare function strMakePrintable(byref a as string) as string
declare function strIsValidSymbolId(byval s as zstring ptr) as integer
declare function strIsNumber(byref s as string) as integer
declare function strIsReservedIdInC(byval id as zstring ptr) as integer
declare function strMatch(byref s as const string, byref pattern as const string) as integer

'' TODO: are EOL nodes really necessary? Maybe it's enough to store an EOL flag
'' on all possible terminal nodes.
type StringMatcher
	enum
		MatchRoot = 0 '' StringMatcher.root will automatically use this (zero-initialization)
		MatchString
		MatchWildcard
		MatchEol
	end enum

	nodeclass as integer

	'' string for MatchString nodes
	text as zstring ptr
	textlength as integer

	'' array of child nodes
	children as StringMatcher ptr
	childcount as integer

	nonEmpty as integer

	declare destructor()
	declare sub addChild(byval nodeclass as integer, byval text as const ubyte ptr, byval textlength as integer)
	declare sub addChildHoldingPreviousChildren(byval nodeclass as integer, byval text as const ubyte ptr, byval textlength as integer)
	declare sub truncateTextToOnly(byval newlength as integer)
	declare sub addPattern(byval pattern as const zstring ptr)
	declare function matches(byval s as const zstring ptr) as integer
	declare function dump1() as string
	declare sub dump()
	declare operator let(byref as const StringMatcher) '' unimplemented
end type

enum
	OS_LINUX
	OS_FREEBSD
	OS_OPENBSD
	OS_NETBSD
	OS_DARWIN
	OS_WINDOWS
	OS_CYGWIN
	OS_DOS
	OS__COUNT
end enum

enum
	ARCH_X86
	ARCH_X86_64
	ARCH_ARM
	ARCH_AARCH64
	ARCH__COUNT
end enum

type OsInfo
	as zstring ptr id, fbdefine
	as byte is_unix, has_64bit, has_arm
end type

type ArchInfo
	id as zstring ptr
	as byte is_64bit, is_arm
end type

extern osinfo(0 to OS__COUNT-1) as OsInfo
extern archinfo(0 to ARCH__COUNT-1) as ArchInfo

type TargetInfo
	as byte os, arch
	declare function id() as string
end type

type ApiBits
	const BitsArrayElements = 4
	const MaxApis = sizeof(ulongint) * BitsArrayElements * 8

	bits(0 to BitsArrayElements-1) as ulongint

	declare function calcAccess(byval api as integer, byref element as integer) as ulongint
	declare sub set(byval api as integer)
	declare sub set(byref rhs as ApiBits)
	declare function isSet(byval api as integer) as integer
	declare function equals(byref rhs as ApiBits) as integer
	declare function coversAtLeast(byref rhs as ApiBits) as integer
	declare function hasAtLeast1Set() as integer
	declare function containsNoneOf(byref rhs as ApiBits) as integer
	declare function dump() as string
end type

''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''

'' The hash table is an array of items,
'' which associate a string to some user data.
type THashItem
	s	as zstring ptr
	hash	as ulong     '' hash value for quick comparison
	data	as any ptr   '' user data
end type

type THash
	items		as THashItem ptr
	count		as integer  '' number of used items
	room		as integer  '' number of allocated items

	'' Whether this hash table should strDuplicate() when storing strings
	'' and free them on hashEnd(). If FALSE, the caller is responsible for
	'' ensuring that strings passed to hashAdd*() stay valid as long as
	'' hashLookup()'s may be done, i.e. typically until hashEnd().
	duplicate_strings	as integer

	declare constructor(byval exponent as integer, byval duplicate_strings as integer = FALSE)
	declare destructor()
	declare operator let(byref as const THash) '' unimplemented

	declare function lookup(byval s as zstring ptr, byval hash as ulong) as THashItem ptr
	declare function lookupDataOrNull(byval id as zstring ptr) as any ptr
	declare function contains(byval s as zstring ptr, byval hash as ulong) as integer
	declare sub add(byval item as THashItem ptr, byval hash as ulong, byval s as zstring ptr, byval dat as any ptr)
	declare function addOverwrite(byval s as zstring ptr, byval dat as any ptr) as THashItem ptr
	#if __FB_DEBUG__
		declare sub dump()
	#endif

	private:
		declare sub allocTable()
		declare sub growTable()
end type

declare function hashHash(byval s as zstring ptr) as ulong

''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''

enum
	FBKW_OP
	FBKW_CORE
	FBKW_QUIRK
	FBKW_RTL
	FBKW_PP
end enum

declare function fbkeywordsLookup(byval id as zstring ptr) as integer

''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''

#if defined(__FB_WIN32__) or defined(__FB_DOS__)
	const PATHDIV = $"\"
#else
	const PATHDIV = "/"
#endif

declare function pathStripExt(byref path as string) as string
declare function pathExtOnly(byref path as string) as string
declare function pathOnly(byref path as string) as string
declare function pathStrip(byref path as string) as string
declare function pathAddDiv(byref path as string) as string
declare function pathIsAbsolute(byref s as string) as integer
declare function pathMakeAbsolute(byref path as string) as string
declare function hExepath() as string
declare function hCurdir() as string
declare function pathStripCurdir(byref path as string) as string
declare function hReadableDirExists(byref path as string) as integer
declare function pathIsDir(byref s as string) as integer
declare function pathNormalize(byref path as string) as string

''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''

const TKFLAG_BEHINDSPACE	= 1 shl 0  '' was preceded by spaces?
const TKFLAG_NOEXPAND		= 1 shl 1  '' may be macro-expanded? (cpp)
const TKFLAG_REMOVE		= 1 shl 2
const TKFLAG_STARTOFDIRECTIVE	= 1 shl 3
const TKFLAG_ROOTFILE		= 1 shl 4  '' Used to mark the internal #include statements which pull in the toplevel files
const TKFLAG_PREINCLUDE		= 1 shl 5
const TKFLAG_DIRECTIVE		= 1 shl 6  '' used to mark #defines/#includes for hMoveDirectivesOutOfConstructs()
const TKFLAG_EXPANSION		= 1 shl 7  '' comes from macro?

enum
	TK_EOF
	TK_STRAYBYTE
	TK_BEGIN
	TK_END
	TK_PPMERGE
	TK_ARGBEGIN
	TK_ARGEND
	TK_EOL
	TK_ENDINCLUDE
	TK_FBCODE

	'' Number/string literals
	TK_NUMBER
	TK_STRING
	TK_CHAR
	TK_WSTRING
	TK_WCHAR

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
	TK_EQ           '' =
	TK_EQEQ         '' ==
	TK_GT           '' >
	TK_GTGT         '' >>
	TK_GTGTEQ       '' >>=
	TK_GTEQ         '' >=
	TK_QUEST        '' ?
	TK_LBRACKET     '' [
	TK_RBRACKET     '' ]
	TK_CIRC         '' ^
	TK_CIRCEQ       '' ^=
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
	KW___ATTRIBUTE = KW__C_FIRST
	KW___ATTRIBUTE__
	KW___INLINE
	KW___INLINE__
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
	KW_INCLUDE_NEXT
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

	OPT_O = OPT__FIRST
	OPT_EMIT
	OPT_DONTEMIT
	OPT_I
	OPT_V
	OPT_TARGET
	OPT_TITLE

	OPT_DECLAREVERSIONS
	OPT_DECLAREBOOL
	OPT_SELECTTARGET
	OPT_SELECTVERSION
	OPT_SELECTDEFINE
	OPT_CASE
	OPT_CASEELSE
	OPT_ENDSELECT
	OPT_IFTARGET
	OPT_IFDEF
	OPT_ELSE
	OPT_ENDIF

	OPT_DEFINE
	OPT_INCLUDE
	OPT_FBFROGINCLUDE
	OPT_INCDIR

	OPT_WINDOWSMS
	OPT_CLONG32
	OPT_FIXUNSIZEDARRAYS
	OPT_DISABLECONSTANTS
	OPT_NOFUNCTIONBODIES
	OPT_DROPMACROBODYSCOPES
	OPT_REPLACEMENTS
	OPT_RENAMETYPEDEF
	OPT_RENAMETAG
	OPT_RENAMEPROC
	OPT_RENAMEDEFINE
	OPT_RENAMEMACROPARAM
	OPT_RENAME
	OPT_REMOVEEMPTYRESERVEDDEFINES
	OPT_REMOVE
	OPT_REMOVEDEFINE
	OPT_REMOVEPROC
	OPT_REMOVEVAR
	OPT_REMOVE1ST
	OPT_REMOVE2ND
	OPT_DROPPROCBODY
	OPT_TYPEDEFHINT
	OPT_ADDFORWARDDECL
	OPT_UNDEFBEFOREDECL
	OPT_IFNDEFDECL
	OPT_CONVBODYTOKENS
	OPT_EXPANDINDEFINE
	OPT_NOEXPAND
	OPT_EXPAND
	OPT_NOSTRING
	OPT_STRING
	OPT_REMOVEINCLUDE
	OPT_SETARRAYSIZE
	OPT_MOVEABOVE
	OPT_INCLIB
	OPT_UNDEF
	OPT_ADDINCLUDE

	OPT__LAST = OPT_ADDINCLUDE

	TK__COUNT
end enum

declare function tkInfoText(byval id as integer) as zstring ptr
declare function tkInfoPretty(byval tk as integer) as string

'' Debugging helper, for example: TRACE(x), "decl begin"
#define TRACE(x) print __FUNCTION__ + "(" + str(__LINE__) + "): " + tkDumpOne(x)

'' original = not from a macro
#define tkIsOriginal(x) ((tkGetFlags(x) and TKFLAG_EXPANSION) = 0)
#define tkIsDirective(x) ((tkGetFlags(x) and TKFLAG_DIRECTIVE) <> 0)
#define tkIsStartOfDirective(x) ((tkGetFlags(x) and TKFLAG_STARTOFDIRECTIVE) <> 0)

declare sub tkInit()
declare sub tkEnd()
declare function tkDumpOne(byval x as integer) as string
declare sub tkDump overload(byval first as integer, byval last as integer)
declare sub tkDump overload()
declare function tkGetCount() as integer
declare sub tkInsert _
	( _
		byval x as integer, _
		byval id as integer, _
		byval text as zstring ptr = NULL _
	)
declare sub tkRemove(byval first as integer, byval last as integer)
declare sub tkCopy _
	( _
		byval x as integer, _
		byval first as integer, _
		byval last as integer, _
		byval flagmask as integer _
	)
declare function tkGet(byval x as integer) as integer
declare function tkGetText(byval x as integer) as zstring ptr
declare function tkSpellId(byval x as integer) as zstring ptr
declare sub tkSetLocation(byval x as integer, byval location as TkLocation)
declare function tkGetLocation(byval x as integer) as TkLocation
declare sub tkSetFlags(byval x as integer, byval flags as integer)
declare sub tkAddFlags(byval first as integer, byval last as integer, byval flags as integer)
declare sub tkSetRemove overload(byval x as integer)
declare sub tkSetRemove overload(byval first as integer, byval last as integer)
declare function tkGetFlags(byval x as integer) as integer
declare sub tkApplyRemoves()
declare sub tkTurnCPPTokensIntoCIds()
declare function tkCTokenRangesAreEqual(byval a as integer, byval b as integer, byval length as integer) as integer
declare function tkSpell overload(byval x as integer) as string
declare function tkSpell overload(byval first as integer, byval last as integer) as string
declare function hFindClosingParen _
	( _
		byval x as integer, _
		byval inside_directive as integer, _
		byval ignore_directive as integer _
	) as integer
declare function tkIsEolOrEof(byval x as integer) as integer
declare function hSkipToEol(byval x as integer) as integer
declare function hSkipConstruct(byval x as integer, byval ignore_directives as integer) as integer
declare function tkReport(byval x as integer, byval message as zstring ptr) as string
declare sub tkOops(byval x as integer, byval message as zstring ptr)
declare function tkButFound(byval x as integer) as string
declare function tkMakeExpectedMessage(byval x as integer, byval message as zstring ptr) as string
declare sub tkOopsExpected(byval x as integer, byval message as zstring ptr)
declare sub tkExpect(byval x as integer, byval tk as integer, byval message as zstring ptr)

''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''

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
#define typeIsConstAt(dt, at) (((dt) and (1 shl (TYPEPOS_CONST + (at)))) <> 0)
#define typeGetDt(dt) ((dt) and TYPEMASK_DT)
#define typeGetDtAndPtr(dt) ((dt) and (TYPEMASK_DT or TYPEMASK_PTR))
#define typeGetPtrCount(dt) (((dt) and TYPEMASK_PTR) shr TYPEPOS_PTR)
#define typeAddrOf(dt) _
	(((dt) and TYPEMASK_DT) or _
	 (((dt) and TYPEMASK_PTR) + (1 shl TYPEPOS_PTR)) or _
	 (((dt) and TYPEMASK_CONST) shl 1))
#define typeMultAddrOf(dt, count) _
	((dt and TYPEMASK_DT) or _
	 ((dt and TYPEMASK_PTR) + (count shl TYPEPOS_PTR)) or _
	 ((dt and TYPEMASK_CONST) shl count))
#define typeGetConst(dt) ((dt) and TYPEMASK_CONST)

declare function typeExpand(byval a as integer, byval b as integer) as integer
declare function typeUnsetBaseConst(byval dtype as integer) as integer

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
	ASTCLASS_PPERROR
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
const ASTATTRIB_NORENAMELIST  = 1 shl 17
const ASTATTRIB_USED          = 1 shl 18
const ASTATTRIB_IFNDEFDECL    = 1 shl 19
const ASTATTRIB_NOSTRING      = 1 shl 20 '' helper flag used during CharStringPass to mark nodes affected by -nostring
const ASTATTRIB_STRING        = 1 shl 21 '' same for -string

const ASTATTRIB__CALLCONV = ASTATTRIB_CDECL or ASTATTRIB_STDCALL

'' When changing, adjust astClone(), astIsEqual(), astDump*()
'' TODO: pack
type ASTNODE
	class		as integer  '' ASTCLASS_*
	attrib		as integer  '' ASTATTRIB_*

	'' Identifiers/string literals, or NULL
	text		as zstring ptr
	alias		as zstring ptr  '' Original identifier, if symbol was renamed

	'' Data type (vars, fields, params, function results, expressions)
	dtype		as integer
	subtype		as ASTNODE ptr
	array		as ASTNODE ptr '' ARRAY holding DIMENSIONs, or NULL
	bits		as ASTNODE ptr '' bitfields only. TODO: change to simple number, FB doesn't support expressions as bitfield size anyways

	'' Initializers, condition expressions, macro/procedure bodies, ...
	expr		as ASTNODE ptr

	location	as TKLOCATION

	union
		paramcount	as integer  '' PPDEFINE: -1 = #define m, 0 = #define m(), 1 = #define m(a), ...
		maxalign	as integer  '' STRUCT/UNION: FIELD=N/#pragma pack(N)
		opt		as integer  '' OPTION: OPT_*
		apis		as ApiBits  '' VERBLOCK
		vernum		as integer  '' VERNUMCHECK: index into frog.vernums()
	end union

	'' Linked list of child nodes, operands/fields/parameters/...
	head		as ASTNODE ptr
	tail		as ASTNODE ptr
	next		as ASTNODE ptr
	prev		as ASTNODE ptr
end type

'' result = boolean = whether to visit this node's children
'' (can be used to skip #define bodies, etc.)
type ASTVISITCALLBACK as function(byval as ASTNODE ptr) as integer

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

declare function astNew overload(byval class_ as integer) as ASTNODE ptr
declare function astNew overload(byval class_ as integer, byval text as zstring ptr) as ASTNODE ptr
declare function astNew overload(byval class_ as integer, byval c1 as ASTNODE ptr, byval c2 as ASTNODE ptr = NULL) as ASTNODE ptr
declare function astNewPPDEFINE(byval id as zstring ptr) as ASTNODE ptr
declare function astNewIIF _
	( _
		byval cond as ASTNODE ptr, _
		byval l as ASTNODE ptr, _
		byval r as ASTNODE ptr _
	) as ASTNODE ptr
declare function astNewGROUP overload() as ASTNODE ptr
declare function astNewGROUP overload(byval c1 as ASTNODE ptr, byval c2 as ASTNODE ptr = NULL) as ASTNODE ptr
declare sub astBuildGroupAndAppend(byref group as ASTNODE ptr, byval n as ASTNODE ptr)
declare function astNewDEFINEDfb64(byval negate as integer) as ASTNODE ptr
declare function astNewDEFINEDfbarm(byval negate as integer) as ASTNODE ptr
declare function astNewDEFINEDfbos(byval os as integer) as ASTNODE ptr
declare function astNewOPTION(byval opt as integer, byval text1 as zstring ptr = NULL, byval text2 as zstring ptr = NULL) as ASTNODE ptr
declare sub astTakeChildren(byval d as ASTNODE ptr, byval s as ASTNODE ptr)
declare sub astTakeAndPrependChildren(byval d as ASTNODE ptr, byval s as ASTNODE ptr)
declare sub astTakeAndAppendChildSequence(byval d as ASTNODE ptr, byval s as ASTNODE ptr, byval first as ASTNODE ptr, byval last as ASTNODE ptr)
declare function astCloneChildren(byval src as ASTNODE ptr) as ASTNODE ptr
declare function astGroupContains(byval group as ASTNODE ptr, byval lookfor as ASTNODE ptr) as integer
declare function astGroupContainsAnyChildrenOf(byval l as ASTNODE ptr, byval r as ASTNODE ptr) as integer
declare function astGroupContainsAllChildrenOf(byval l as ASTNODE ptr, byval r as ASTNODE ptr) as integer
declare sub astDelete(byval n as ASTNODE ptr)
declare sub astInsert(byval parent as ASTNODE ptr, byval n as ASTNODE ptr, byval ref as ASTNODE ptr)
declare sub astPrepend(byval parent as ASTNODE ptr, byval n as ASTNODE ptr)
declare sub astAppend(byval parent as ASTNODE ptr, byval n as ASTNODE ptr)
declare sub astUnlink(byval parent as ASTNODE ptr, byval n as ASTNODE ptr)
declare function astRemove(byval parent as ASTNODE ptr, byval a as ASTNODE ptr) as ASTNODE ptr
declare sub astRemoveChildren(byval parent as ASTNODE ptr)
declare function astReplace _
	( _
		byval parent as ASTNODE ptr, _
		byval old as ASTNODE ptr, _
		byval n as ASTNODE ptr _
	) as ASTNODE ptr
declare sub astSetText(byval n as ASTNODE ptr, byval text as zstring ptr)
declare sub astSetAlias(byval n as ASTNODE ptr, byval alias_ as zstring ptr)
declare sub astRenameSymbol(byval n as ASTNODE ptr, byval newid as zstring ptr, byval add_to_renamelist as integer = TRUE)
declare sub astSetType _
	( _
		byval n as ASTNODE ptr, _
		byval dtype as integer, _
		byval subtype as ASTNODE ptr _
	)
declare function astCloneNode(byval n as ASTNODE ptr) as ASTNODE ptr
declare function astClone(byval n as ASTNODE ptr) as ASTNODE ptr
declare function astContains(byval n as ASTNODE ptr, byval astclass as integer) as integer
declare function astContainsCAssignments(byval n as ASTNODE ptr) as integer
declare function astHas1Child(byval n as ASTNODE ptr) as integer
declare function astHasOnlyChild(byval n as ASTNODE ptr, byval astclass as integer) as integer
declare function astIsCodeBlock(byval n as ASTNODE ptr) as integer
declare function astIsMergableBlock(byval n as ASTNODE ptr) as integer
declare function astIsCastTo(byval n as ASTNODE ptr, byval dtype as integer, byval subtype as ASTNODE ptr) as integer
declare function astIsEqual _
	( _
		byval a as ASTNODE ptr, _
		byval b as ASTNODE ptr, _
		byval is_merge as integer = FALSE _
	) as integer
declare function hGetFbNumberLiteralPrefix(byval attrib as integer) as string
declare function astEvalConstiAsInt64(byval n as ASTNODE ptr) as longint
declare function astIsConst0(byval n as ASTNODE ptr) as integer
declare function astLookupMacroParam(byval macro as ASTNODE ptr, byval id as zstring ptr) as integer
declare sub astVisit(byval n as ASTNODE ptr, byval callback as ASTVISITCALLBACK)
declare function astCount(byval n as ASTNODE ptr) as integer
declare function astDumpPrettyClass(byval astclass as integer) as string
declare function astDumpPrettyDecl(byval n as ASTNODE ptr, byval show_type as integer = FALSE) as string
declare function astDumpOne(byval n as ASTNODE ptr) as string
declare sub astDump _
	( _
		byval n as ASTNODE ptr, _
		byval nestlevel as integer = 0, _
		byref prefix as string = "" _
	)

declare function astDumpPrettyVersion(byval n as ASTNODE ptr) as string
declare function astNewVERAND(byval a as ASTNODE ptr = NULL, byval b as ASTNODE ptr = NULL) as ASTNODE ptr
declare function astNewVEROR(byval a as ASTNODE ptr = NULL, byval b as ASTNODE ptr = NULL) as ASTNODE ptr
declare function astNewVERNUMCHECK(byval vernum as integer) as ASTNODE ptr
declare function astEmitVerNumCheck(byval n as ASTNODE ptr, byref eqsign as string) as string
declare function astMergeVerblocks _
	( _
		byval a as ASTNODE ptr, _
		byval b as ASTNODE ptr _
	) as ASTNODE ptr
declare sub astMergeNext(byval api as ApiBits, byref final as ASTNODE ptr, byref incoming as ASTNODE ptr)
declare sub astProcessVerblocks(byval code as ASTNODE ptr)

''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''

type IndexPattern
	parentpattern as string
	childindex as integer
	declare function determineParentId(byval parentparent as ASTNODE ptr, byval parent as ASTNODE ptr) as const zstring ptr
	declare function matches _
		( _
			byval parentparent as ASTNODE ptr, _
			byval parent as ASTNODE ptr, _
			byval childindex as integer _
		) as integer
end type

type DeclPatterns
	stringPatterns as StringMatcher
	indexPatterns as IndexPattern ptr
	indexPatternCount as integer
	declare sub parseAndAdd(byref s as string)
	declare destructor()
	declare function matches _
		( _
			byval parentparent as ASTNODE ptr, _
			byval parent as ASTNODE ptr, _
			byval child as ASTNODE ptr, _
			byval childindex as integer _
		) as integer
	declare operator let(byref as const DeclPatterns) '' unimplemented
end type

type CodeReplacement
	as zstring ptr fromcode, tocode
	tofb as integer
	patternlen as integer '' used temporarily by hApplyReplacements()
end type

type ApiInfo
	verand as ASTNODE ptr
	script as ASTNODE ptr

	target as TargetInfo
	as integer windowsms, clong32, fixunsizedarrays, disableconstants
	as integer nofunctionbodies, dropmacrobodyscopes, removeEmptyReservedDefines

	have_renames as integer
	renameopt(OPT_RENAMETYPEDEF to OPT_RENAME) as THash = any
	idopt(OPT_REMOVE to OPT_EXPAND) as StringMatcher
	patterns(OPT_NOSTRING to OPT_STRING) as DeclPatterns
	removeinclude as THash = THash(3, FALSE)
	setarraysizeoptions as THash = THash(3, FALSE)
	moveaboveoptions as ASTNODE ptr

	replacements as CodeReplacement ptr
	replacementcount as integer

	log as ASTNODE ptr

	declare constructor()
	declare destructor()
	declare sub addReplacement(byval fromcode as zstring ptr, byval tocode as zstring ptr, byval tofb as integer)
	declare sub loadOption(byval opt as integer, byval param1 as zstring ptr, byval param2 as zstring ptr)
	declare sub loadOptions()
	declare sub print(byref ln as string)
	declare function prettyId() as string
	declare operator let(byref as const ApiInfo) '' unimplemented
end type

''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''

type HeaderInfo
	title as string
	as FileBuffer ptr licensefile, translatorsfile
end type

declare function lexLoadC(byval x as integer, byval code as zstring ptr, byref source as SourceInfo) as integer
declare function lexLoadArgs(byval x as integer, byval args as zstring ptr, byref source as SourceInfo) as integer

declare function emitType overload _
	( _
		byval dtype as integer, _
		byval subtype as ASTNODE ptr, _
		byval debugdump as integer = FALSE _
	) as string
declare function emitType overload(byval n as ASTNODE ptr) as string
declare function emitExpr(byval n as ASTNODE ptr, byval need_parens as integer = FALSE, byval need_macroparam_parens as integer = TRUE) as string
declare sub emitFile(byref filename as string, byval header as HeaderInfo ptr, byval ast as ASTNODE ptr)
declare sub emitStdout(byval ast as ASTNODE ptr, byval indent as integer)

type COperatorInfo
	precedence as byte
	is_right_assoc as byte
end type

extern copinfo(ASTCLASS_CLOGOR to ASTCLASS_IIF) as COperatorInfo

#define cprecedence(op) copinfo(op).precedence
#define cOpIsLeftAssoc(op) (not copinfo(op).is_right_assoc)

declare function hNumberLiteral _
	( _
		byval x as integer, _
		byval is_cpp as integer, _
		byref errmsg as string _
	) as ASTNODE ptr
declare function hStringLiteral _
	( _
		byval x as integer, _
		byval eval_escapes as integer, _
		byref errmsg as string _
	) as ASTNODE ptr
declare function hDefineHead(byref x as integer) as ASTNODE ptr

declare sub cppInit(byref api as ApiInfo)
declare sub cppEnd()
declare sub cppAddPredefine(byval id as zstring ptr, byval body as zstring ptr)
declare sub cppAddTargetPredefines(byval target as TargetInfo)
declare sub cppAddIncDir(byval incdir as zstring ptr)
declare sub cppAppendIncludeDirective(byval filename as zstring ptr, byval tkflags as integer)
declare sub cppMain()
declare sub hMoveDirectivesOutOfConstructs()
declare sub hApplyReplacements()

declare sub cInit(byref api as ApiInfo)
declare sub cEnd()
declare function cMain() as ASTNODE ptr

type ApiSpecificBiOptions
	as ASTNODE ptr inclibs, undefs, addincludes
end type

declare sub hlAutoAddDividers(byval ast as ASTNODE ptr)
declare sub hlGlobal(byval ast as ASTNODE ptr, byref api as ApiInfo)
declare sub hlFile(byval ast as ASTNODE ptr, byref api as ApiInfo, byref bioptions as ApiSpecificBiOptions)
declare function hlCountDecls(byval ast as ASTNODE ptr) as integer
declare function hlCountTodos(byval ast as ASTNODE ptr) as integer

''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''

namespace frog
	extern as integer verbose
	extern as integer enabledoscount
	extern as ASTNODE ptr script
	extern as ASTNODE ptr completeverors, fullveror
	extern as ApiInfo ptr apis
	extern as integer apicount
	extern as ApiBits fullapis
	extern vernums(any) as string
	extern versiondefine as string
end namespace

declare function frogLookupBiFromH(byval hfile as zstring ptr) as integer
