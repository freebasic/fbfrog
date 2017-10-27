#include once "common.bi"
#include once "lex.bi"

namespace tktokens

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
	KW___ASM = KW__C_FIRST
	KW___ASM__
	KW___ATTRIBUTE
	KW___ATTRIBUTE__
	KW___INLINE
	KW___INLINE__
	KW___RESTRICT
	KW___RESTRICT__
	KW__BOOL
	KW__PRAGMA
	KW_ASM
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
	OPT_RENAME_
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
	OPT_FORCEFUNCTION2MACRO
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

end namespace

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
