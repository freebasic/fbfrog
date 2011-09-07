#include once "tk.bi"
#include once "common.bi"

#if defined(__FB_WIN32__) or defined(__FB_DOS__)
	#define EOL_CHARS !"\r\n"
#else
	#define EOL_CHARS !"\n"
#endif

'' Same order as TK_*/KW_* enum
'' Here we only need those tokens that we might emit as FB code.
'' For some tokens (such as string literals) the text payload that they
'' carry around will be used instead of this table, and some are pure C
'' tokens that should never be emitted into FB code at all, both of these
'' kinds have a NULL here.
dim shared as zstring ptr tokentext(0 to (TK__COUNT - 1)) = _
{ _
	NULL, _       '' TK_EOF
	NULL, _       '' TK_TODO
	NULL, _       '' TK_BYTE
	@EOL_CHARS, _ '' TK_EOL
	NULL, _       '' TK_COMMENT
	NULL, _       '' TK_LINECOMMENT
	_ '' Number literals
	NULL, _
	NULL, _
	NULL, _
	_ '' String literals
	NULL, _
	NULL, _
	NULL, _
	NULL, _
	NULL, _
	NULL, _
	NULL, _
	NULL, _
	_ '' Main tokens
	NULL      , _ '' !
	@"<>"     , _ '' !=
	@"#"      , _
	@"##"     , _
	@"mod"    , _ '' %
	@"mod="   , _ '' %=
	@"and"    , _ '' &
	@"and="   , _ '' &=
	@"andalso", _ '' &&
	@"("      , _
	@")"      , _
	@"*"      , _
	@"*="     , _
	@"+"      , _
	@"+="     , _
	NULL      , _ '' ++
	@","      , _
	@"-"      , _
	@"-="     , _
	NULL      , _ '' --
	@"->"     , _
	@"."      , _
	@"..."    , _
	@"/"      , _
	@"/="     , _
	@":"      , _
	NULL      , _ '' ;
	@"<"      , _
	@"shl"    , _ '' <<
	@"shl="   , _ '' <<=
	@"<="     , _
	@"="      , _
	@"="      , _ '' ==
	@">"      , _
	@"shr"    , _ '' >>
	@"shr="   , _ '' >>=
	@">="     , _
	NULL      , _ '' ?
	@"["      , _
	@"\"      , _
	@"]"      , _
	@"xor"    , _ '' ^
	@"xor="   , _ '' ^=
	@"{"      , _
	@"or"     , _ '' |
	@"or="    , _ '' |=
	@"orelse" , _ '' ||
	@"}"      , _
	@"not"    , _ '' ~
	_
	NULL, _ '' TK_ID
	_ '' C/FB keywords
	NULL         , _ '' auto
	NULL         , _ '' break
	@"case"      , _
	NULL         , _ '' char
	@"const"     , _
	@"continue"  , _
	NULL         , _ '' default
	@"define"    , _
	@"defined"   , _
	@"do"        , _
	@"double"    , _
	NULL         , _ '' elif
	@"else"      , _
	@"endif"     , _
	@"enum"      , _
	@"extern"    , _
	NULL         , _ '' float
	@"for"       , _
	@"goto"      , _
	@"if"        , _
	@"ifdef"     , _
	@"ifndef"    , _
	@"include"   , _
	NULL         , _ '' inline
	NULL         , _ '' int
	@"long"      , _
	@"pragma"    , _
	NULL         , _ '' register
	NULL         , _ '' restrict
	@"return"    , _
	@"short"     , _
	NULL         , _ '' signed
	@"sizeof"    , _
	@"static"    , _
	NULL         , _ '' struct
	NULL         , _ '' switch
	NULL         , _ '' typedef
	@"undef"     , _
	@"union"     , _
	NULL         , _ '' unsigned
	NULL         , _ '' void
	NULL         , _ '' volatile
	@"while"     , _
	_ '' FB-only keywords
	@"alias"      , _
	@"as"         , _
	@"byte"       , _
	@"byval"      , _
	@"cast"       , _
	@"cdecl"      , _
	@"cptr"       , _
	@"declare"    , _
	@"dim"        , _
	@"elseif"     , _
	@"end"        , _
	@"exit"       , _
	@"export"     , _
	@"field"      , _
	@"function"   , _
	@"iif"        , _
	@"integer"    , _
	@"longint"    , _
	@"loop"       , _
	@"next"       , _
	@"pascal"     , _
	@"private"    , _
	@"ptr"        , _
	@"scope"      , _
	@"select"     , _
	@"shared"     , _
	@"single"     , _
	@"stdcall"    , _
	@"sub"        , _
	@"then"       , _
	@"to"         , _
	@"type"       , _
	@"ubyte"      , _
	@"uinteger"   , _
	@"ulong"      , _
	@"ulongint"   , _
	@"ushort"     , _
	@"wend"       , _
	@"wstr"       , _
	@"wstring"    , _
	@"zstring"      _
}

type EmitterStuff
	as integer fo '' Output file
end type

dim shared as EmitterStuff emit

private sub flush(byval text as zstring ptr)
	dim as integer length = len(*text)
	if (put(#emit.fo, , *cptr(ubyte ptr, text), length)) then
		xoops("file I/O failed")
	end if
end sub

sub tk_emit_file(byref filename as string)
	emit.fo = freefile()
	if (open(filename, for binary, access write, as #emit.fo)) then
		xoops("could not open output file: '" & filename & "'")
	end if

	for i as integer = 0 to (tk_count() - 1)
		dim as integer id = tk_get(i)
		dim as zstring ptr text = tk_text(i)
		if (text = NULL) then
			text = tokentext(id)
			if (text = NULL) then
				text = @"/' TODO '/"
			end if
		end if
		flush(text)
	next

	close #emit.fo
	emit.fo = 0
end sub
