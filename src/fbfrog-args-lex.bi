#include once "lex.bi"

type ArgLexer extends LexContext
	declare constructor()
	declare operator let(byref as const ArgLexer) '' unimplemented
	declare sub readArg(byval t as integer)
end type

declare function lexLoadArgs(byref tk as TokenBuffer, byval x as integer, byval args as zstring ptr, byval source as const SourceInfo ptr) as integer
