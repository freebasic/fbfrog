'' Token buffer (implemented as a gap buffer), accessor functions

#include once "fbfrog.bi"
#include once "crt.bi"

type TOKENINFO
	text		as zstring ptr
	debug		as zstring ptr
end type

dim shared as TOKENINFO tk_info(0 to ...) = _
{ _
	( NULL  , @"eof"      ), _
	( NULL  , @"divider"  ), _
	( NULL  , @"ppinclude" ), _
	( NULL  , @"ppdefine" ), _
	( NULL  , @"ppif"     ), _
	( NULL  , @"ppelseif" ), _
	( NULL  , @"ppelse"   ), _
	( NULL  , @"ppendif"  ), _
	( NULL  , @"ppundef"  ), _
	( NULL  , @"ppunknown" ), _
	( NULL  , @"begin"    ), _
	( NULL  , @"end"      ), _
	( NULL  , @"byte"     ), _
	( NULL  , @"space"    ), _
	( NULL  , @"eol"      ), _
	( NULL  , @"comment"  ), _
	( NULL  , @"decnum"   ), _ '' Number literals
	( NULL  , @"hexnum"   ), _
	( NULL  , @"octnum"   ), _
	( NULL  , @"decfloat" ), _
	( NULL  , @"string"   ), _ '' String literals
	( NULL  , @"char"     ), _
	( NULL  , @"wstring"  ), _
	( NULL  , @"wchar"    ), _
	( NULL  , @"estring"  ), _
	( NULL  , @"echar"    ), _
	( NULL  , @"ewstring" ), _
	( NULL  , @"ewchar"   ), _
	( @"!"  , @"tk" ), _ '' Main tokens
	( @"!=" , @"tk" ), _
	( @"#"  , @"tk" ), _
	( @"##" , @"tk" ), _
	( @"%"  , @"tk" ), _
	( @"%=" , @"tk" ), _
	( @"&"  , @"tk" ), _
	( @"&=" , @"tk" ), _
	( @"&&" , @"tk" ), _
	( @"("  , @"tk" ), _
	( @")"  , @"tk" ), _
	( @"*"  , @"tk" ), _
	( @"*=" , @"tk" ), _
	( @"+"  , @"tk" ), _
	( @"+=" , @"tk" ), _
	( @"++" , @"tk" ), _
	( @","  , @"tk" ), _
	( @"-"  , @"tk" ), _
	( @"-=" , @"tk" ), _
	( @"--" , @"tk" ), _
	( @"->" , @"tk" ), _
	( @"."  , @"tk" ), _
	( @"...", @"tk" ), _
	( @"/"  , @"tk" ), _
	( @"/=" , @"tk" ), _
	( @":"  , @"tk" ), _
	( @";"  , @"tk" ), _
	( @"<"  , @"tk" ), _
	( @"<<" , @"tk" ), _
	( @"<<=", @"tk" ), _
	( @"<=" , @"tk" ), _
	( @"<>" , @"tk" ), _
	( @"="  , @"tk" ), _
	( @"==" , @"tk" ), _
	( @">"  , @"tk" ), _
	( @">>" , @"tk" ), _
	( @">>=", @"tk" ), _
	( @">=" , @"tk" ), _
	( @"?"  , @"tk" ), _
	( @"@"  , @"tk" ), _
	( @"["  , @"tk" ), _
	( @"\"  , @"tk" ), _
	( @"]"  , @"tk" ), _
	( @"^"  , @"tk" ), _
	( @"^=" , @"tk" ), _
	( @"_"  , @"tk" ), _
	( @"{"  , @"tk" ), _
	( @"|"  , @"tk" ), _
	( @"|=" , @"tk" ), _
	( @"||" , @"tk" ), _
	( @"}"  , @"tk" ), _
	( @"~"  , @"tk" ), _
	( NULL  , @"id" ), _ '' TK_ID
	( @"__attribute__", @"kw" ), _ '' C-only keywords
	( @"__cdecl" , @"kw" ), _
	( @"__restrict", @"kw" ), _
	( @"__restrict__", @"kw" ), _
	( @"__stdcall", @"kw" ), _
	( @"auto"    , @"kw" ), _
	( @"break"   , @"kw" ), _
	( @"char"    , @"kw" ), _
	( @"default" , @"kw" ), _
	( @"elif"    , @"kw" ), _
	( @"float"   , @"kw" ), _
	( @"inline"  , @"kw" ), _
	( @"register", @"kw" ), _
	( @"restrict", @"kw" ), _
	( @"signed"  , @"kw" ), _
	( @"struct"  , @"kw" ), _
	( @"switch"  , @"kw" ), _
	( @"typedef" , @"kw" ), _
	( @"void"    , @"kw" ), _
	( @"volatile", @"kw" ), _
	( @"case"    , @"kw" ), _  '' C/FB shared keywords
	( @"const"   , @"kw" ), _
	( @"continue", @"kw" ), _
	( @"define"  , @"kw" ), _
	( @"defined" , @"kw" ), _
	( @"do"      , @"kw" ), _
	( @"double"  , @"kw" ), _
	( @"else"    , @"kw" ), _
	( @"endif"   , @"kw" ), _
	( @"enum"    , @"kw" ), _
	( @"extern"  , @"kw" ), _
	( @"for"     , @"kw" ), _
	( @"goto"    , @"kw" ), _
	( @"if"      , @"kw" ), _
	( @"ifdef"   , @"kw" ), _
	( @"ifndef"  , @"kw" ), _
	( @"include" , @"kw" ), _
	( @"int"     , @"kw" ), _
	( @"long"    , @"kw" ), _
	( @"pragma"  , @"kw" ), _
	( @"return"  , @"kw" ), _
	( @"short"   , @"kw" ), _
	( @"sizeof"  , @"kw" ), _
	( @"static"  , @"kw" ), _
	( @"undef"   , @"kw" ), _
	( @"union"   , @"kw" ), _
	( @"unsigned", @"kw" ), _
	( @"while"   , @"kw" ), _
	( @"alias"   , @"kw" ), _  '' FB-only keywords
	( @"and"     , @"kw" ), _
	( @"andalso" , @"kw" ), _
	( @"any"     , @"kw" ), _
	( @"as"      , @"kw" ), _
	( @"byte"    , @"kw" ), _
	( @"byval"   , @"kw" ), _
	( @"cast"    , @"kw" ), _
	( @"cdecl"   , @"kw" ), _
	( @"cptr"    , @"kw" ), _
	( @"declare" , @"kw" ), _
	( @"dim"     , @"kw" ), _
	( @"elseif"  , @"kw" ), _
	( @"end"     , @"kw" ), _
	( @"exit"    , @"kw" ), _
	( @"export"  , @"kw" ), _
	( @"field"   , @"kw" ), _
	( @"function", @"kw" ), _
	( @"iif"     , @"kw" ), _
	( @"integer" , @"kw" ), _
	( @"longint" , @"kw" ), _
	( @"loop"    , @"kw" ), _
	( @"mod"     , @"kw" ), _
	( @"next"    , @"kw" ), _
	( @"not"     , @"kw" ), _
	( @"or"      , @"kw" ), _
	( @"orelse"  , @"kw" ), _
	( @"pascal"  , @"kw" ), _
	( @"private" , @"kw" ), _
	( @"ptr"     , @"kw" ), _
	( @"scope"   , @"kw" ), _
	( @"select"  , @"kw" ), _
	( @"shared"  , @"kw" ), _
	( @"shl"     , @"kw" ), _
	( @"shr"     , @"kw" ), _
	( @"single"  , @"kw" ), _
	( @"stdcall" , @"kw" ), _
	( @"sub"     , @"kw" ), _
	( @"then"    , @"kw" ), _
	( @"to"      , @"kw" ), _
	( @"type"    , @"kw" ), _
	( @"typeof"  , @"kw" ), _
	( @"ubyte"   , @"kw" ), _
	( @"uinteger", @"kw" ), _
	( @"ulong"   , @"kw" ), _
	( @"ulongint", @"kw" ), _
	( @"ushort"  , @"kw" ), _
	( @"wend"    , @"kw" ), _
	( @"wstr"    , @"kw" ), _
	( @"wstring" , @"kw" ), _
	( @"xor"     , @"kw" ), _
	( @"zstring" , @"kw" )  _
}

#assert ubound( tk_info ) = TK__COUNT - 1

function tkInfoText( byval tk as integer ) as zstring ptr
	function = tk_info(tk).text
end function

''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''

type ONETOKEN
	id		as short  '' TK_*

	'' Tokens from unknown constructs that couldn't be parsed successfully
	'' will be marked poisoned, so the parser can avoid them in the 2nd run
	poisoned	as short

	text		as zstring ptr  '' Identifiers/literals, or NULL
	ast		as ASTNODE ptr  '' for TK_PP* high level tokens
	linenum		as integer      '' where this token was found
	comment		as zstring ptr
end type

type TKBUFFER
	'' Gap buffer of tokens
	p		as ONETOKEN ptr  '' Buffer containing: front,gap,back
	front		as integer  '' Front length; the gap's offset
	gap		as integer  '' Gap length
	size		as integer  '' Front + back

	'' Static EOF token for out-of-bounds accesses
	eof		as ONETOKEN
end type

dim shared as TKBUFFER tk

sub tkInit( )
	tk.p = NULL
	tk.front = 0
	tk.gap = 0
	tk.size = 0

	tk.eof.id = TK_EOF
	tk.eof.poisoned = FALSE
	tk.eof.text = NULL
	tk.eof.ast = NULL
	tk.eof.linenum = -1
	tk.eof.comment = NULL
end sub

private function tkAccess( byval x as integer ) as ONETOKEN ptr
	'' Inside end?
	if( x >= tk.front ) then
		'' Invalid?
		if( x >= tk.size ) then
			return @tk.eof
		end if
		x += tk.gap
	else
		'' Invalid?
		if( x < 0 ) then
			return @tk.eof
		end if
	end if

	function = tk.p + x
end function

sub tkEnd( )
	tkRemove( 0, tk.size - 1 )
	deallocate( tk.p )
end sub

function tkDumpBasic( byval id as integer, byval text as zstring ptr ) as string
	var s = "[" + *tk_info(id).debug + "]"
	s += " "
	if( text ) then
		s += "'" + *text + "'"
	elseif( tk_info(id).text ) then
		s += "'" + *tk_info(id).text + "'"
	end if
	function = s
end function

function tkDumpOne( byval x as integer ) as string
	dim as ONETOKEN ptr p = any
	dim as string s, comment
	dim as zstring ptr text = any

	p = tkAccess( x )
	s += str( x ) + " "
	s += "["
	s += *tk_info(p->id).debug
	if( p->poisoned ) then
		s += " posioned"
	end if
	s += "]"

	s += " "
	if( p->text ) then
		s += "'" + *p->text + "'"
	elseif( tk_info(p->id).text ) then
		s += "'" + *tk_info(p->id).text + "'"
	end if

	text = tkGetComment( x )
	if( text ) then
		comment = strReplace( *text, !"\n", "\n" )
		if( len( comment ) > 40 ) then
			comment = left( comment, 40 ) + "..."
		end if
		s += " comment(" + comment + ")"
	end if

	if( tkGetAst( x ) ) then
		s += "ast=" & astDumpInline( tkGetAst( x ) )
	end if

	function = s
end function

sub tkDump( )
	for i as integer = 0 to tk.size - 1
		print tkDumpOne( i )
	next
end sub

private sub hMoveTo( byval x as integer )
	dim as integer old = any
	dim as ONETOKEN ptr p = any

	if( x < 0 ) then
		x = 0
	elseif( x > tk.size ) then
		x = tk.size
	end if

	old = tk.front
	if( x < old ) then
		'' Move gap left
		p = tk.p + x
		memmove( p + tk.gap, p, (old - x) * sizeof( ONETOKEN ) )
	elseif( x > old ) then
		'' Move gap right
		p = tk.p + old
		memmove( p, p + tk.gap, (x - old) * sizeof( ONETOKEN ) )
	end if

	tk.front = x
end sub

function tkGetCount( ) as integer
	function = tk.size
end function

'' Insert new token in front of token at the given position,
'' so that the new token ends up at that position
sub tkInsert _
	( _
		byval x as integer, _
		byval id as integer, _
		byval text as zstring ptr, _
		byval ast as ASTNODE ptr _
	)

	const NEWGAP = 512
	dim as ONETOKEN ptr p = any

	'' Move gap in front of the position
	hMoveTo( x )

	'' Make room for the new data, if necessary
	if( tk.gap = 0 ) then
		'' Reallocate the buffer, then move the back block to the
		'' end of the new buffer, so that the gap in the middle grows.
		tk.p = reallocate( tk.p, (tk.size + NEWGAP) * sizeof( ONETOKEN ) )
		p = tk.p + tk.front
		if( tk.size > tk.front ) then
			memmove( p + NEWGAP, p + tk.gap, _
			         (tk.size - tk.front) * sizeof( ONETOKEN ) )
		end if
		tk.gap = NEWGAP
	else
		p = tk.p + tk.front
	end if

	p->id = id
	p->poisoned = FALSE
	p->text = strDuplicate( text )
	p->ast = ast
	p->linenum = -1
	p->comment = NULL

	'' Extend front part of the buffer
	tk.front += 1
	tk.gap -= 1
	tk.size += 1

end sub

sub tkRemove( byval first as integer, byval last as integer )
	if( first < 0 ) then first = 0
	if( last >= tk.size ) then last = tk.size - 1
	if( first > last ) then exit sub

	for i as integer = first to last
		var p = tkAccess( i )
		deallocate( p->text )
		astDelete( p->ast )
		deallocate( p->comment )
	next

	var delta = last - first + 1

	'' Gap is in front of first token to delete?
	if( tk.front = first ) then
		'' Then do a forward deletion
		assert( delta <= (tk.size - tk.front) )
	else
		'' Otherwise, move the gap behind the last token,
		'' and do a backwards deletion
		hMoveTo( last + 1 )
		assert( delta <= tk.front )
		tk.front -= delta
	end if

	tk.gap += delta
	tk.size -= delta
end sub

'' Copy tokens first..last and insert them in front of x
sub tkCopy( byval x as integer, byval first as integer, byval last as integer )
	if( first < 0 ) then first = 0
	if( last >= tk.size ) then last = tk.size - 1
	if( first > last ) then exit sub
	if( (x < 0) or (x > tk.size) ) then exit sub
	assert( (x <= first) or (x > last) )

	do
		var src = tkAccess( first )
		tkInsert( x, src->id, src->text )
		'' Careful when inserting before the source range, the position
		'' offsets shift by 1 everytime
		if( x <= first ) then
			first += 1
			last += 1
		end if

		src = tkAccess( first )
		var dst = tkAccess( x )
		dst->poisoned = src->poisoned
		dst->ast = astClone( src->ast )
		dst->linenum = src->linenum
		dst->comment = strDuplicate( src->comment )

		x += 1
		first += 1
	loop while( first <= last )
end sub

function tkGet( byval x as integer ) as integer
	function = tkAccess( x )->id
end function

function tkGetText( byval x as integer ) as zstring ptr
	function = tkAccess( x )->text
end function

function tkGetIdOrKw( byval x as integer ) as zstring ptr
	var p = tkAccess( x )
	assert( p->id >= TK_ID )
	if( p->text ) then
		function = p->text
	else
		function = tk_info(p->id).text
	end if
end function

function tkGetAst( byval x as integer ) as ASTNODE ptr
	function = tkAccess( x )->ast
end function

sub tkSetAst( byval x as integer, byval ast as ASTNODE ptr )
	var p = tkAccess( x )
	astDelete( p->ast )
	p->ast = ast
end sub

sub tkSetPoisoned( byval first as integer, byval last as integer )
	for i as integer = first to last
		var p = tkAccess( i )
		if( p->id <> TK_EOF ) then
			p->poisoned = TRUE
		end if
	next
end sub

function tkIsPoisoned( byval x as integer ) as integer
	function = tkAccess( x )->poisoned
end function

sub tkSetLineNum( byval x as integer, byval linenum as integer )
	var p = tkAccess( x )
	if( p->id <> TK_EOF ) then
		p->linenum = linenum
	end if
end sub

function tkGetLineNum( byval x as integer ) as integer
	function = tkAccess( x )->linenum
end function

sub tkSetComment( byval x as integer, byval comment as zstring ptr )
	dim as ONETOKEN ptr p = any
	p = tkAccess( x )
	if( p->id <> TK_EOF ) then
		deallocate( p->comment )
		p->comment = strDuplicate( comment )
	end if
end sub

function tkGetComment( byval x as integer ) as zstring ptr
	function = tkAccess( x )->comment
end function

function tkCount _
	( _
		byval tk as integer, _
		byval first as integer, _
		byval last as integer _
	) as integer

	dim as integer count = any

	count = 0

	for i as integer = first to last
		if( tkGet( i ) = tk ) then
			count += 1
		end if
	next

	function = count
end function

function tkSkipSpaceAndComments _
	( _
		byval x as integer, _
		byval delta as integer _
	) as integer

	do
		x += delta

		select case( tkGet( x ) )
		case TK_SPACE, TK_COMMENT

		case else
			exit do
		end select
	loop

	function = x
end function

function tkToCText( byval id as integer, byval text as zstring ptr ) as string
	select case as const( id )
	case TK_DIVIDER, TK_EOL : function = !"\n"
	case TK_BEGIN, TK_END   :
	case TK_BYTE            : function = *text
	case TK_SPACE           : function = " "
	case TK_COMMENT         : function = "/* " + *text + " */"
	case TK_DECNUM          : function = *text
	case TK_HEXNUM          : function = "0x" + *text
	case TK_OCTNUM          : function = "0" + *text
	case TK_DECFLOAT        : function = *text
	case TK_STRING, TK_ESTRING   : function = """" + *text + """"
	case TK_CHAR, TK_ECHAR       : function = "'" + *text + "'"
	case TK_WSTRING, TK_EWSTRING : function = "L""" + *text + """"
	case TK_WCHAR, TK_EWCHAR     : function = "L'" + *text + "'"
	case TK_EXCL to TK_TILDE     : function = *tk_info(id).text
	case TK_ID                   : function = *text
	case KW__C_FIRST to KW__C_LAST
		function = *tk_info(id).text
	case else
		assert( FALSE )
	end select
end function

function tkManyToCText _
	( _
		byval first as integer, _
		byval last as integer _
	) as string

	dim as string s

	for i as integer = first to last
		var p = tkAccess( i )

		'' This shouldn't be used with high-level tokens; that'd be
		'' pretty difficult
		assert( p->ast = NULL )

		s += tkToCText( p->id, p->text )
	next

	function = s
end function

function tkToAstText _
	( _
		byval first as integer, _
		byval last as integer _
	) as ASTNODE ptr

	dim as string s
	dim as ASTNODE ptr group, text

	for i as integer = first to last
		var p = tkAccess( i )
		if( p->ast ) then
			if( len( s ) > 0 ) then
				text = astNew( ASTCLASS_TEXT, s )
			end if
			if( group = NULL ) then
				group = astNew( ASTCLASS_GROUP )
			end if
			astAppend( group, text )
			astAppend( group, astClone( p->ast ) )
			text = NULL
			s = ""
		else
			s += tkToCText( p->id, p->text )
		end if
	next

	if( len( s ) > 0 ) then
		text = astNew( ASTCLASS_TEXT, s )
	end if

	if( group ) then
		astAppend( group, text )
		function = group
	else
		function = text
	end if
end function

function tkCollectComments _
	( _
		byval first as integer, _
		byval last as integer _
	) as string

	dim as string s
	dim as zstring ptr text = any

	'' Collect all comment text from a range of tokens and merge it into
	'' one string, which can be used
	for i as integer = first to last
		if( tkGet( i ) = TK_COMMENT ) then
			text = tkGetText( i )
		else
			text = tkGetComment( i )
		end if

		if( text ) then
			if( len( s ) > 0 ) then
				s += !"\n"
			end if
			s += *text
		end if
	next

	function = s
end function

sub tkRemoveAllOf( byval id as integer, byval text as zstring ptr )
	dim as integer match = any
	for x as integer = 0 to tkGetCount( )-1
		if( tkGet( x ) = id ) then
			match = TRUE
			if( text ) then
				match = (*tkGetText( x ) = *text)
			end if
			if( match ) then
				tkRemove( x, x )
				x -= 1
			end if
		end if
	next
end sub
