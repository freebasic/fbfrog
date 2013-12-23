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
	( NULL  , @"pperror"  ), _
	( NULL  , @"ppwarning" ), _
	( NULL  , @"begin"    ), _
	( NULL  , @"end"      ), _
	( NULL  , @"ppmerge"  ), _
	( NULL  , @"emptymacroparam" ), _
	( NULL  , @"eol"      ), _
	( NULL  , @"comment"  ), _
	( NULL  , @"begininclude" ), _
	( NULL  , @"endinclude" ), _
	( NULL  , @"decnum"   ), _ '' Number literals
	( NULL  , @"hexnum"   ), _
	( NULL  , @"octnum"   ), _
	( NULL  , @"decfloat" ), _
	( NULL  , @"string"   ), _ '' String literals
	( NULL  , @"char"     ), _
	( NULL  , @"wstring"  ), _
	( NULL  , @"wchar"    ), _
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
	( @"__attribute__", NULL ), _ '' C keywords
	( @"__cdecl"      , NULL ), _
	( @"__restrict"   , NULL ), _
	( @"__restrict__" , NULL ), _
	( @"__stdcall"    , NULL ), _
	( @"auto"    , NULL ), _
	( @"break"   , NULL ), _
	( @"case"    , NULL ), _
	( @"char"    , NULL ), _
	( @"const"   , NULL ), _
	( @"continue", NULL ), _
	( @"default" , NULL ), _
	( @"define"  , NULL ), _
	( @"defined" , NULL ), _
	( @"do"      , NULL ), _
	( @"double"  , NULL ), _
	( @"elif"    , NULL ), _
	( @"else"    , NULL ), _
	( @"endif"   , NULL ), _
	( @"enum"    , NULL ), _
	( @"error"   , NULL ), _
	( @"extern"  , NULL ), _
	( @"float"   , NULL ), _
	( @"for"     , NULL ), _
	( @"goto"    , NULL ), _
	( @"if"      , NULL ), _
	( @"ifdef"   , NULL ), _
	( @"ifndef"  , NULL ), _
	( @"include" , NULL ), _
	( @"inline"  , NULL ), _
	( @"int"     , NULL ), _
	( @"long"    , NULL ), _
	( @"pragma"  , NULL ), _
	( @"register", NULL ), _
	( @"restrict", NULL ), _
	( @"return"  , NULL ), _
	( @"short"   , NULL ), _
	( @"signed"  , NULL ), _
	( @"sizeof"  , NULL ), _
	( @"static"  , NULL ), _
	( @"struct"  , NULL ), _
	( @"switch"  , NULL ), _
	( @"typedef" , NULL ), _
	( @"undef"   , NULL ), _
	( @"union"   , NULL ), _
	( @"unsigned", NULL ), _
	( @"void"    , NULL ), _
	( @"volatile", NULL ), _
	( @"warning" , NULL ), _
	( @"while"   , NULL )  _
}

#assert ubound( tk_info ) = TK__COUNT - 1

function tkInfoText( byval id as integer ) as zstring ptr
	function = tk_info(id).text
end function

''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''

type ONETOKEN
	id		as short  '' TK_*
	behindspace	as short  '' whether this token was preceded by spaces

	'' TK_ID: Identifier
	''
	'' TK_STRING: String literal's content with escape sequences solved out,
	'' except for \\ and \0 (so it can still be represented as
	'' null-terminated string)
	''
	'' TK_DECNUM/TK_HEXNUM/TK_OCTNUM: Original token text without octal/hex
	'' prefixes ('0' or '0x'), this is enough for
	''    - parsing code to easily retrieve the integer values by doing
	''      valulng( "&h" + *text )
	''    - CPP code to differentiate '0', '0x', '0x0', etc. when doing
	''      ## merging
	''
	'' rest: NULL
	text		as zstring ptr

	'' TK_PP* high level tokens
	ast		as ASTNODE ptr

	location	as TKLOCATION   '' where this token was found
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

	#if 0
	if( p->location.file ) then
		s += " location: " + p->location.file->pretty + "(" & p->location.linenum+1 & ") column=" & p->location.column & " length=" & p->location.length
	end if
	#endif

	function = s
end function

sub tkDump( )
	for i as integer = 0 to tk.size - 1
		print tkDumpOne( i )
	next
end sub

private sub hMoveTo( byval x as integer )
	if( x < 0 ) then
		x = 0
	elseif( x > tk.size ) then
		x = tk.size
	end if

	var old = tk.front
	if( x < old ) then
		'' Move gap left
		var p = tk.p + x
		memmove( p + tk.gap, p, (old - x) * sizeof( ONETOKEN ) )
	elseif( x > old ) then
		'' Move gap right
		var p = tk.p + old
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
		byval text as zstring ptr _
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

	clear( *p, 0, sizeof( *p ) )
	p->id = id
	p->text = strDuplicate( text )

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
		dst->behindspace = src->behindspace
		dst->ast = astClone( src->ast )
		dst->location = src->location
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
		byval text as zstring ptr _
	)

	var lastin1stline = first
	for i as integer = first+2 to last
		select case( tkGet( i ) )
		case TK_EOL, TK_DIVIDER, TK_PPINCLUDE, TK_PPDEFINE, TK_PPIF, _
		     TK_PPELSEIF, TK_PPELSE, TK_PPENDIF, TK_PPUNDEF, TK_PPERROR, TK_PPWARNING
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
	tkInsert( first, id, text )
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

	var count = 0

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
	for x as integer = 0 to tkGetCount( )-1
		if( tkGet( x ) = id ) then
			var match = TRUE
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

sub tkRemoveEOLs( )
	var x = 0
	do
		select case( tkGet( x ) )
		case TK_EOF
			exit do
		case TK_EOL
			tkRemove( x, x )
			x -= 1
		end select
		x += 1
	loop
end sub

sub tkTurnCPPTokensIntoCIds( )
	for x as integer = 0 to tkGetCount( )-1
		var tk = tkGet( x )
		select case( tk )
		case KW_DEFINE, KW_INCLUDE, KW_ELIF, KW_IFDEF, KW_IFNDEF, _
		     KW_ENDIF, KW_UNDEF, KW_PRAGMA, KW_ERROR, KW_WARNING
			tkRemove( x, x )
			tkInsert( x, TK_ID, tkInfoText( tk ) )
		end select
	next
end sub

''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''

private function hMakePrettyCStrLit( byval text as zstring ptr ) as string
	dim s as string

	do
		select case( (*text)[0] )
		case 0
			exit do

		'' Internal format: can contain \\ and \0 escape sequences to
		'' encode embedded null chars
		case CH_BACKSLASH
			text += 1
			assert( (text[0] = CH_BACKSLASH) or (text[0] = CH_0) )
			s += "\"
			s += chr( (*text)[0] )

		case CH_DQUOTE    : s += "\"""
		case CH_QUOTE     : s += "\'"
		case CH_QUEST     : s += "\?"
		case CH_BELL      : s += "\a"
		case CH_BACKSPACE : s += "\b"
		case CH_FORMFEED  : s += "\f"
		case CH_LF        : s += "\n"
		case CH_CR        : s += "\r"
		case CH_TAB       : s += "\t"
		case CH_VTAB      : s += "\v"
		case is < 32, 127 : s += "\" + oct( (*text)[0] )
		case else         : s += chr( (*text)[0] )
		end select

		text += 1
	loop

	function = s
end function

private function hMakePrettyCTokenText _
	( _
		byval id as integer, _
		byval text as zstring ptr _
	) as string

	select case as const( id )
	case TK_PPENDIF   : function = "#endif"
	case TK_DECNUM    : function = *text
	case TK_HEXNUM    : function = "0x" + *text
	case TK_OCTNUM    : function = "0" + *text
	case TK_DECFLOAT  : function = *text
	case TK_STRING    : function = """" + hMakePrettyCStrLit( *text ) + """"
	case TK_CHAR      : function = "'" + hMakePrettyCStrLit( *text ) + "'"
	case TK_WSTRING   : function = "L""" + hMakePrettyCStrLit( *text ) + """"
	case TK_WCHAR     : function = "L'" + hMakePrettyCStrLit( *text ) + "'"
	case TK_EXCL to TK_TILDE : function = *tk_info(id).text
	case TK_ID               : function = *text
	case KW__C_FIRST to KW__C_LAST
		function = *tk_info(id).text
	end select

end function

sub tkReport _
	( _
		byval x as integer, _
		byval message as zstring ptr, _
		byval more_context as integer _
	)

	var location = tkGetLocation( x )
	if( location->filename ) then
		hReportLocation( location, message, more_context )
	else
		TRACE( x ), "<= error here"
		print string( 40, "-" )
		const CONTEXT = 10
		for i as integer = x - CONTEXT to x + CONTEXT
			if( (i >= 0) and (i < tk.size) ) then
				print tkDumpOne( i )
			end if
		next
		print string( 40, "-" )
		print *message
	end if

end sub

sub tkOops( byval x as integer, byval message as zstring ptr )
	tkReport( x, message )
	end 1
end sub

sub tkOopsExpected _
	( _
		byval x as integer, _
		byval message as zstring ptr, _
		byval whatfor as zstring ptr _
	)

	dim s as string

	select case( tkGet( x ) )
	case TK_EOL, TK_EOF, TK_DIVIDER
		s = "missing " + *message
		if( whatfor ) then s += " " + *whatfor
	case else
		var found = "'" + hMakePrettyCTokenText( tkGet( x ), tkGetText( x ) ) + "'"
		if( len( found ) > 0 ) then
			found = " but found " + found
		end if
		s = "expected " + *message
		if( whatfor ) then s += " " + *whatfor
		s += found
	end select

	tkOops( x, s )
end sub

sub tkExpect _
	( _
		byval x as integer, _
		byval tk as integer, _
		byval whatfor as zstring ptr _
	)

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

		tkOopsExpected( x, expected, whatfor )
	end if

end sub
