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
	( NULL  , @"ast"      ), _
	( NULL  , @"begin"    ), _
	( NULL  , @"end"      ), _
	( NULL  , @"byte"     ), _
	( NULL  , @"space"    ), _
	( NULL  , @"eol"      ), _
	( NULL  , @"comment"  ), _
	( NULL  , @"decnum"   ), _ '' Number literals
	( NULL  , @"hexnum"   ), _
	( NULL  , @"octnum"   ), _
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
	( @"auto"    , @"kw" ), _ '' C keywords
	( @"break"   , @"kw" ), _
	( @"case"    , @"kw" ), _
	( @"char"    , @"kw" ), _
	( @"const"   , @"kw" ), _
	( @"continue", @"kw" ), _
	( @"default" , @"kw" ), _
	( @"define"  , @"kw" ), _
	( @"defined" , @"kw" ), _
	( @"do"      , @"kw" ), _
	( @"double"  , @"kw" ), _
	( @"elif"    , @"kw" ), _
	( @"else"    , @"kw" ), _
	( @"endif"   , @"kw" ), _
	( @"enum"    , @"kw" ), _
	( @"extern"  , @"kw" ), _
	( @"float"   , @"kw" ), _
	( @"for"     , @"kw" ), _
	( @"goto"    , @"kw" ), _
	( @"if"      , @"kw" ), _
	( @"ifdef"   , @"kw" ), _
	( @"ifndef"  , @"kw" ), _
	( @"include" , @"kw" ), _
	( @"inline"  , @"kw" ), _
	( @"int"     , @"kw" ), _
	( @"long"    , @"kw" ), _
	( @"pragma"  , @"kw" ), _
	( @"register", @"kw" ), _
	( @"restrict", @"kw" ), _
	( @"return"  , @"kw" ), _
	( @"short"   , @"kw" ), _
	( @"signed"  , @"kw" ), _
	( @"sizeof"  , @"kw" ), _
	( @"static"  , @"kw" ), _
	( @"struct"  , @"kw" ), _
	( @"switch"  , @"kw" ), _
	( @"typedef" , @"kw" ), _
	( @"undef"   , @"kw" ), _
	( @"union"   , @"kw" ), _
	( @"unsigned", @"kw" ), _
	( @"void"    , @"kw" ), _
	( @"volatile", @"kw" ), _
	( @"while"   , @"kw" )  _
}

#if ubound( tk_info ) < TK__COUNT - 1
#error "please update the tk_info() table!"
#endif

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
 	ast		as ASTNODE ptr  '' for TK_AST
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

type TKSTATS
	maxsize		as integer  '' Highest amount of tokens at once
	lookups		as integer
	moved		as integer
end type

dim shared as TKBUFFER tk
dim shared as TKSTATS stats

function strDuplicate( byval s as zstring ptr ) as zstring ptr
	dim as zstring ptr p = any
	if( s ) then
		p = callocate( len( *s ) + 1 )
		*p = *s
		function = p
	else
		function = NULL
	end if
end function

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
	stats.lookups += 1

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

sub tkStats( )
	print "tokens: " & _
		stats.maxsize & " max, " & _
		stats.lookups & " lookups, " & _
		stats.moved & " moved"
end sub

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

	function = s
end function

sub tkDump( )
	for i as integer = 0 to tk.size - 1
		print tkDumpOne( i )
		if( tkGet( i ) = TK_AST ) then
			astDump( tkGetAst( i ), 2 )
		end if
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
		stats.moved += old - x
	elseif( x > old ) then
		'' Move gap right
		p = tk.p + old
		memmove( p, p + tk.gap, (x - old) * sizeof( ONETOKEN ) )
		stats.moved += x - old
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
			stats.moved += tk.size - tk.front
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

	if( stats.maxsize < tk.size ) then
		stats.maxsize = tk.size
	end if

end sub

sub tkRemove( byval first as integer, byval last as integer )
	dim as ONETOKEN ptr p = any
	dim as integer delta = any

	if( first < 0 ) then
		first = 0
	end if

	if( last >= tk.size ) then
		last = tk.size - 1
	end if

	if( first > last ) then
		exit sub
	end if

	for i as integer = first to last
		p = tkAccess( i )
		deallocate( p->text )
		astDelete( p->ast )
		deallocate( p->comment )
	next

	delta = last - first + 1

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

function tkGet( byval x as integer ) as integer
	function = tkAccess( x )->id
end function

function tkGetText( byval x as integer ) as zstring ptr
	function = tkAccess( x )->text
end function

function tkGetAst( byval x as integer ) as ASTNODE ptr
	function = tkAccess( x )->ast
end function

sub tkSetPoisoned( byval first as integer, byval last as integer )
	dim as ONETOKEN ptr p = any
	for i as integer = first to last
		p = tkAccess( i )
		if( p->id <> TK_EOF ) then
			p->poisoned = TRUE
		end if
	next
end sub

function tkIsPoisoned( byval x as integer ) as integer
	function = tkAccess( x )->poisoned
end function

sub tkSetLineNum( byval x as integer, byval linenum as integer )
	dim as ONETOKEN ptr p = any
	p = tkAccess( x )
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

function tkToText( byval first as integer, byval last as integer ) as string
	dim as ONETOKEN ptr p = any
	dim as string s

	for i as integer = first to last
		p = tkAccess( i )

		'' Some tokens carry their own text (e.g. identifiers)
		if( p->text ) then
			s += *p->text
		else
			'' For others, lookup in the info table
			if( p->id >= TK_EXCL ) then
				assert( p->id <> TK_ID )
				s += *tk_info(p->id).text
			end if
		end if
	next

	function = s
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
