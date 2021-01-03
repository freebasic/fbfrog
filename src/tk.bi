#include once "common.bi"
#include once "source.bi"

'' Whether the token was preceded by effective white-space (e.g. spaces, tabs, comments...).
'' Needed e.g. for C pre-processing to differentiate between <#define M (x)> and <#define M(x)>.
const TKFLAG_BEHINDSPACE	= 1 shl 0

'' Whether to skip macro expansion for this identifier token.
'' The C pre-processor uses this to disable expansion of identifiers in a macro
'' body matching the macro name, preventing recursive macro expansion.
const TKFLAG_NOEXPAND		= 1 shl 1

'' Tokens can be marked for removal later. This is used e.g. by the C
'' pre-processor to remove the preprocessor directives except #defines and
'' #pragma after it has finished preprocessing. It's easier to just remove the
'' tokens that were marked for removal, instead of parsing again.
const TKFLAG_REMOVE		= 1 shl 2

'' Used to mark the # token from the beginning of C preprocessor directives.
const TKFLAG_STARTOFDIRECTIVE	= 1 shl 3

'' Used to mark the internal #include statements which pull in the toplevel files
'' or pre-#includes specified on the command line. This is needed because we
'' add #include statements to the token stream to implement these features,
'' but want to differentiate those artificial #include statements from the
'' original ones in the input stream.
const TKFLAG_ROOTFILE		= 1 shl 4
const TKFLAG_PREINCLUDE		= 1 shl 5

'' Used to mark #defines/#includes for hMoveDirectivesOutOfConstructs()
const TKFLAG_DIRECTIVE		= 1 shl 6

'' Whether the token originated from a macro expansion. This is used by the C
'' pre-processor to skip directive parsing for # tokens from macro expansions.
const TKFLAG_EXPANSION		= 1 shl 7

namespace tktokens

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

#assert tktokens.TK__COUNT <= &hFF

'' This should be as small as possible, to reduce memory usage and the amount of
'' memmove()/memcpy() required for insertions/deletions in the gap buffer.
type ONETOKEN
	'' TK_ID: Identifier
	''
	'' TK_STRING: C string literal as-is, preserving quotes and escape
	'' sequences solved out, except for escaped EOLs.
	''
	'' TK_DECNUM/TK_HEXNUM/TK_OCTNUM: Original token text without octal/hex
	'' prefixes ('0' or '0x'), this is enough for
	''    - parsing code to easily retrieve the integer values by doing
	''      valulng("&h" + *text)
	''    - CPP code to differentiate '0', '0x', '0x0', etc. when doing
	''      ## merging
	''
	'' rest: NULL
	text		as zstring ptr

	location	as TkLocation   '' where this token was found

	id		as ubyte  '' TK_*
	flags		as ubyte  '' TKFLAG_*
end type
#assert sizeof(ONETOKEN) = sizeof(any ptr) + 8

type TokenBuffer
private:
	sourcectx as SourceContext ptr

	'' Gap buffer of tokens
	buffer as ONETOKEN ptr  '' Buffer containing: front,gap,back
	front as integer  '' Front length; the gap's offset
	gap as integer  '' Gap length
	size as integer  '' Front + back

	newgapsize as integer  '' Size to use for new gap when increasing the buffer size

	declare function lookup(byval x as integer) as ONETOKEN ptr
	declare sub moveTo(byval x as integer)

public:
	declare constructor(byval sourcectx as SourceContext ptr)
	declare destructor()
	declare function dumpOne(byval x as integer) as string
	declare sub dump(byval first as integer, byval last as integer)
	declare sub dump()
	declare function count() as integer
	declare sub insert(byval x as integer, byval id as integer, byval text as zstring ptr = NULL)
	declare sub remove(byval first as integer, byval last as integer)
	declare sub copy(byval x as integer, byval first as integer, byval last as integer, byval flagmask as integer)
	declare function get(byval x as integer) as integer
	declare function getText(byval x as integer) as zstring ptr
	declare function spellId(byval x as integer) as zstring ptr
	declare sub setLocation(byval x as integer, byval location as TkLocation)
	declare function getLocation(byval x as integer) as TkLocation
	declare sub setFlags(byval x as integer, byval flags as integer)
	declare sub addFlags(byval first as integer, byval last as integer, byval flags as integer)
	declare sub setRemove(byval x as integer)
	declare sub setRemove(byval first as integer, byval last as integer)
	declare function getFlags(byval x as integer) as integer
	declare sub applyRemoves()
	declare function isOriginal(byval x as integer) as integer
	declare function isDirective(byval x as integer) as integer
	declare function isStartOfDirective(byval x as integer) as integer

	declare function isKwThatShouldBecomeId(byval x as integer) as integer
	declare sub turnCPPTokensIntoCIds()
	declare function areCTokenRangesEqual(byval a as integer, byval b as integer, byval length as integer) as integer

	declare function spell(byval x as integer) as string
	declare function spell(byval first as integer, byval last as integer) as string

	declare function findClosingParen(byval x as integer, byval inside_directive as integer, byval ignore_directive as integer) as integer
	declare function isEolOrEof(byval x as integer) as integer
	declare function skipToEol(byval x as integer) as integer
	declare function skipConstruct(byval x as integer, byval ignore_directives as integer) as integer
	declare sub findConstructBoundaries(byval x as integer, byref first as integer, byref last as integer)
	declare function reportConstructTokens(byval x as integer, byval first as integer, byval last as integer) as string
	declare function report(byval x as integer, byval message as zstring ptr) as string
	declare sub showErrorAndAbort(byval x as integer, byval message as zstring ptr)
	declare function butFound(byval x as integer) as string
	declare function makeExpectedMessage(byval x as integer, byval something as zstring ptr) as string
	declare sub oopsExpected(byval x as integer, byval message as zstring ptr)
	declare sub expect(byval x as integer, byval tk as integer, byval message as zstring ptr)
end type

declare function tkInfoText(byval id as integer) as zstring ptr
declare function tkInfoPretty(byval tk as integer) as string
