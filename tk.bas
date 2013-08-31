'' Token buffer (implemented as a gap buffer), accessor functions

#include once "fbfrog.bi"
#include once "crt.bi"

type TOKENINFO
	text		as zstring ptr
	debug		as zstring ptr
	pretty		as zstring ptr
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
	( NULL  , @"begin"    ), _
	( NULL  , @"end"      ), _
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
	( @"!"  , NULL ), _ '' Main tokens
	( @"!=" , NULL ), _
	( @"#"  , NULL ), _
	( @"##" , NULL ), _
	( @"%"  , NULL ), _
	( @"%=" , NULL ), _
	( @"&"  , NULL ), _
	( @"&=" , NULL ), _
	( @"&&" , NULL ), _
	( @"("  , NULL ), _
	( @")"  , NULL ), _
	( @"*"  , NULL ), _
	( @"*=" , NULL ), _
	( @"+"  , NULL ), _
	( @"+=" , NULL ), _
	( @"++" , NULL ), _
	( @","  , NULL ), _
	( @"-"  , NULL ), _
	( @"-=" , NULL ), _
	( @"--" , NULL ), _
	( @"->" , NULL ), _
	( @"."  , NULL ), _
	( @"...", NULL ), _
	( @"/"  , NULL ), _
	( @"/=" , NULL ), _
	( @":"  , NULL ), _
	( @";"  , NULL ), _
	( @"<"  , NULL ), _
	( @"<<" , NULL ), _
	( @"<<=", NULL ), _
	( @"<=" , NULL ), _
	( @"<>" , NULL ), _
	( @"="  , NULL ), _
	( @"==" , NULL ), _
	( @">"  , NULL ), _
	( @">>" , NULL ), _
	( @">>=", NULL ), _
	( @">=" , NULL ), _
	( @"?"  , NULL ), _
	( @"@"  , NULL ), _
	( @"["  , NULL ), _
	( @"\"  , NULL ), _
	( @"]"  , NULL ), _
	( @"^"  , NULL ), _
	( @"^=" , NULL ), _
	( @"_"  , NULL ), _
	( @"{"  , NULL ), _
	( @"|"  , NULL ), _
	( @"|=" , NULL ), _
	( @"||" , NULL ), _
	( @"}"  , NULL ), _
	( @"~"  , NULL ), _
	( NULL  , @"id" ), _ '' TK_ID
	( @"__attribute__", NULL ), _ '' C-only keywords
	( @"__cdecl"      , NULL ), _
	( @"__restrict"   , NULL ), _
	( @"__restrict__" , NULL ), _
	( @"__stdcall"    , NULL ), _
	( @"auto"    , NULL ), _
	( @"break"   , NULL ), _
	( @"char"    , NULL ), _
	( @"default" , NULL ), _
	( @"elif"    , NULL ), _
	( @"float"   , NULL ), _
	( @"inline"  , NULL ), _
	( @"register", NULL ), _
	( @"restrict", NULL ), _
	( @"signed"  , NULL ), _
	( @"struct"  , NULL ), _
	( @"switch"  , NULL ), _
	( @"typedef" , NULL ), _
	( @"void"    , NULL ), _
	( @"volatile", NULL ), _
	( @"case"    , NULL ), _  '' C/FB shared keywords
	( @"const"   , NULL ), _
	( @"continue", NULL ), _
	( @"define"  , NULL ), _
	( @"defined" , NULL ), _
	( @"do"      , NULL ), _
	( @"double"  , NULL ), _
	( @"else"    , NULL ), _
	( @"endif"   , NULL ), _
	( @"enum"    , NULL ), _
	( @"extern"  , NULL ), _
	( @"for"     , NULL ), _
	( @"goto"    , NULL ), _
	( @"if"      , NULL ), _
	( @"ifdef"   , NULL ), _
	( @"ifndef"  , NULL ), _
	( @"include" , NULL ), _
	( @"int"     , NULL ), _
	( @"long"    , NULL ), _
	( @"pragma"  , NULL ), _
	( @"return"  , NULL ), _
	( @"short"   , NULL ), _
	( @"sizeof"  , NULL ), _
	( @"static"  , NULL ), _
	( @"undef"   , NULL ), _
	( @"union"   , NULL ), _
	( @"unsigned", NULL ), _
	( @"while"   , NULL ), _
	( @"alias"   , NULL ), _  '' FB-only keywords
	( @"and"     , NULL ), _
	( @"andalso" , NULL ), _
	( @"any"     , NULL ), _
	( @"as"      , NULL ), _
	( @"byte"    , NULL ), _
	( @"byval"   , NULL ), _
	( @"cast"    , NULL ), _
	( @"cdecl"   , NULL ), _
	( @"cptr"    , NULL ), _
	( @"declare" , NULL ), _
	( @"dim"     , NULL ), _
	( @"elseif"  , NULL ), _
	( @"end"     , NULL ), _
	( @"exit"    , NULL ), _
	( @"export"  , NULL ), _
	( @"field"   , NULL ), _
	( @"function", NULL ), _
	( @"iif"     , NULL ), _
	( @"integer" , NULL ), _
	( @"longint" , NULL ), _
	( @"loop"    , NULL ), _
	( @"macro"   , NULL ), _
	( @"mod"     , NULL ), _
	( @"next"    , NULL ), _
	( @"not"     , NULL ), _
	( @"option"  , NULL ), _
	( @"or"      , NULL ), _
	( @"orelse"  , NULL ), _
	( @"pascal"  , NULL ), _
	( @"private" , NULL ), _
	( @"ptr"     , NULL ), _
	( @"scope"   , NULL ), _
	( @"select"  , NULL ), _
	( @"shared"  , NULL ), _
	( @"shl"     , NULL ), _
	( @"shr"     , NULL ), _
	( @"single"  , NULL ), _
	( @"stdcall" , NULL ), _
	( @"sub"     , NULL ), _
	( @"then"    , NULL ), _
	( @"to"      , NULL ), _
	( @"type"    , NULL ), _
	( @"typeof"  , NULL ), _
	( @"ubyte"   , NULL ), _
	( @"uinteger", NULL ), _
	( @"ulong"   , NULL ), _
	( @"ulongint", NULL ), _
	( @"ushort"  , NULL ), _
	( @"wend"    , NULL ), _
	( @"wstr"    , NULL ), _
	( @"wstring" , NULL ), _
	( @"xor"     , NULL ), _
	( @"zstring" , NULL ), _
	( @"copyfile", NULL ), _  '' fbfrog-only keywords
	( @"download", NULL ), _
	( @"extract" , NULL ), _
	( @"expand"  , NULL ), _
	( @"file"    , NULL ), _
	( @"dir"     , NULL ), _
	( @"remove"  , NULL ), _
	( @"version" , NULL )  _
}

#assert ubound( tk_info ) = TK__COUNT - 1

function tkInfoText( byval id as integer ) as zstring ptr
	function = tk_info(id).text
end function

''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''

type ONETOKEN
	id		as short  '' TK_*
	text		as zstring ptr  '' Identifiers/literals, or NULL
	ast		as ASTNODE ptr  '' for TK_PP* high level tokens
	location	as TKLOCATION   '' where this token was found
	behindspace	as integer      '' whether this token was preceded by spaces
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
	clear( tk, 0, sizeof( tk ) )
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
	var s = "["
	if( tk_info(id).debug ) then
		s += *tk_info(id).debug
	else
		s += *tk_info(id).text
	end if
	if( text ) then
		s += " """ + strMakePrintable( *text ) + """"
	end if
	s += "]"
	function = s
end function

function hDumpComment( byval comment as zstring ptr ) as string
	if( comment ) then
		var scomment = strMakePrintable( *comment )
		if( len( scomment ) > 40 ) then
			scomment = left( scomment, 40 ) + "..."
		end if
		function = " comment=""" + scomment + """"
	end if
end function

function tkDumpOne( byval x as integer ) as string
	var p = tkAccess( x )
	var s = str( x ) + " " + tkDumpBasic( p->id, p->text )

	s += hDumpComment( p->comment )

	if( tkGetAst( x ) ) then
		s += " ast=" & astDumpInline( tkGetAst( x ) )
	end if

	if( p->location.file ) then
		s += " location: " + p->location.file->pretty + "(" & p->location.linenum+1 & ") column=" & p->location.column & " length=" & p->location.length
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
	p->text = strDuplicate( text )
	p->ast = ast
	p->location.file = NULL
	p->location.linenum = 0
	p->location.column = 0
	p->location.length = 0
	p->behindspace = FALSE
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
		dst->ast = astClone( src->ast )
		dst->location = src->location
		dst->behindspace = src->behindspace
		dst->comment = strDuplicate( src->comment )

		x += 1
		first += 1
	loop while( first <= last )
end sub

'' Combine first..last tokens into a single new one
sub tkFold _
	( _
		byval first as integer, _
		byval last as integer, _
		byval id as integer, _
		byval text as zstring ptr, _
		byval ast as ASTNODE ptr _
	)

	var lastin1stline = first
	for i as integer = first+2 to last
		select case( tkGet( i ) )
		case TK_EOL, TK_DIVIDER, TK_PPINCLUDE, TK_PPDEFINE, TK_PPIF, _
		     TK_PPELSEIF, TK_PPELSE, TK_PPENDIF, TK_PPUNDEF
			lastin1stline = i - 1
			exit for
		end select
	next

	'' Sum up all the token lengths and space in between, resulting in one
	'' big combined token; unless it spans across multiple lines.
	var location = *tkGetLocation( first )
	var lastloc  = tkGetLocation( lastin1stline )
	location.length = lastloc->column + lastloc->length - location.column

	'' Insert first - the text/ast pointers may reference one of the tokens
	'' that will be removed; this way we can be sure they're valid when
	'' accessed by tkInsert()
	tkInsert( first, id, text, ast )
	tkSetLocation( first, @location )
	first += 1
	last += 1

	tkRemove( first, last )

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

sub tkSetLocation( byval x as integer, byval location as TKLOCATION ptr )
	var p = tkAccess( x )
	if( p->id <> TK_EOF ) then
		p->location = *location
	end if
end sub

function tkGetLocation( byval x as integer ) as TKLOCATION ptr
	function = @(tkAccess( x )->location)
end function

sub tkSetBehindSpace( byval x as integer )
	var p = tkAccess( x )
	if( p->id <> TK_EOF ) then
		p->behindspace = TRUE
	end if
end sub

function tkGetBehindSpace( byval x as integer ) as integer
	function = tkAccess( x )->behindspace
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

function tkSkipComment _
	( _
		byval x as integer, _
		byval delta as integer _
	) as integer

	do
		x += delta

		select case( tkGet( x ) )
		case TK_COMMENT

		case else
			exit do
		end select
	loop

	function = x
end function

function tkSkipCommentEol _
	( _
		byval x as integer, _
		byval delta as integer _
	) as integer

	do
		x += delta

		select case( tkGet( x ) )
		case TK_COMMENT, TK_EOL

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
	case TK_COMMENT         : function = "/* " + *text + " */"
	case TK_PPENDIF         : function = "#endif"
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
		print tkDumpBasic( id, text )
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
				text = astNewTEXT( s )
			end if
			if( group = NULL ) then
				group = astNewGROUP( )
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
		text = astNewTEXT( s )
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

''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''

sub tkOops( byval x as integer, byref message as string )
	var location = tkGetLocation( x )
	if( location->file ) then
		oopsLocation( location, message )
	else
		TRACE( x ), "<= error here"
		print string( 40, "-" )
		tkDump( )
		print string( 40, "-" )
		print message
		end 1
	end if
end sub

sub tkOopsExpected( byval x as integer, byref message as string )
	select case( tkGet( x ) )
	case TK_EOL, TK_EOF, TK_DIVIDER
		tkOops( x, "missing " + message )
	case else
		var found = "'" + tkToCText( tkGet( x ), tkGetText( x ) ) + "'"
		tkOops( x, "expected " + message + " but found " + found )
	end select
end sub

sub tkExpect( byval x as integer, byval tk as integer )
	if( tkGet( x ) <> tk ) then
		dim as string expected

		select case( tk )
		case TK_EOF    : expected = "end of file"
		case TK_EOL    : expected = "end of line"
		case TK_ID     : expected = "identifier"
		case TK_STRING : expected = """..."" string literal"
		case else
			if( tk_info(tk).debug ) then
				expected = *tk_info(tk).debug
			else
				expected = "'" + *tk_info(tk).text + "'"
			end if
		end select

		tkOopsExpected( x, expected )
	end if
end sub
