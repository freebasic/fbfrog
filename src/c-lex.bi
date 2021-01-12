#include once "lex.bi"

type CLexer extends LexContext
	'' +2 extra room to allow for some "overflowing" to reduce the amount
	'' of checking needed, +1 for null terminator.
	const MAXTEXTLEN = 1 shl 12
	text as zstring * MAXTEXTLEN+2+1

	declare constructor()
	declare operator let(byref as const CLexer) '' unimplemented
	declare sub addTextToken(byval t as integer, byval begin as ubyte ptr)
	declare sub readBytes(byval t as integer, byval length as integer)
	declare sub skipLineComment()
	declare sub skipComment()
	declare sub readId()
	declare sub readNumber()
	declare function skipEscapedEol() as integer
	declare sub readString()
	declare sub tokenize()
	declare sub readArg(byval t as integer)
end type

declare function lexLoadC(byref sourcectx as SourceContext, byref tk as TokenBuffer, byval x as integer, byval source as SourceInfo ptr) as integer
