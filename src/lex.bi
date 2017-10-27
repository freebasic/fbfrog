#include once "util.bi"
#include once "util-hash.bi"

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

const MAXTEXTLEN = 1 shl 12

type LexContext
	i		as ubyte ptr  '' Current char, will always be <= limit
	x		as integer
	location	as TkLocation
	behindspace	as integer

	ckeywords	as THash = THash(12)
	frogoptions	as THash = THash(12)

	'' +2 extra room to allow for some "overflowing" to reduce the amount
	'' of checking needed, +1 for null terminator.
	text		as zstring * MAXTEXTLEN+2+1

	declare constructor()
	declare operator let(byref as const LexContext) '' declared but not implemented

	declare sub oops(byref message as string)
	declare sub setLocation(byval flags as integer = 0)
	declare sub addTextToken(byval t as integer, byval begin as ubyte ptr)
	declare sub readBytes(byval t as integer, byval length as integer)
	declare sub newLine()
	declare sub skipLineComment()
	declare sub skipComment()
	declare sub readId()
	declare sub readNumber()
	declare function skipEscapedEol() as integer
	declare sub readString()
	declare sub tokenize()
	declare sub readArg(byval t as integer)
end type

declare function lexLoadC(byval x as integer, byval code as zstring ptr, byref source as SourceInfo) as integer
declare function lexLoadArgs(byval x as integer, byval args as zstring ptr, byref source as SourceInfo) as integer
