#include once "lex.bi"

type ArgLexer extends LexContext
	declare constructor()
	declare operator let(byref as const ArgLexer) '' unimplemented
	declare sub readArg(byval t as integer)
end type

declare function lexLoadArgs(byref sourcectx as SourceContext, byref tk as TokenBuffer, byval x as integer, byval source as SourceInfo ptr) as integer
