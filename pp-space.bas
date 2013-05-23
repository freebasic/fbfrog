''
'' ppComments() removes TK_COMMENTs, for easier parsing later. It can assign
'' their text to other tokens (tkSetComment()) if comments should be preserved
'' for later.
''
'' ppDividers() merges empty lines (i.e. multiple TK_EOLs separated only by
'' TK_SPACEs) into TK_DIVIDERs, for nicer output formatting later. It can be
'' nice to preserve the block/section/paragraph layout of the input, and still
'' trim down unnecessary newlines.
''

#include once "fbfrog.bi"

private function hIsBeforeEol _
	( _
		byval x as integer, _
		byval delta as integer _
	) as integer

	function = TRUE

	'' Can we reach EOL before hitting any non-space token?
	do
		x += delta

		select case( tkGet( x ) )
		case TK_SPACE, TK_COMMENT

		case TK_EOL, TK_EOF
			exit do

		case TK_AST, TK_DIVIDER
			'' High-level tokens count as separate lines
			exit do

		case else
			function = FALSE
			exit do
		end select
	loop

end function

private sub hAccumComment( byval x as integer, byref comment as string )
	dim as zstring ptr s = any
	dim as string text

	if( len( comment ) = 0 ) then
		exit sub
	end if

	s = tkGetComment( x )
	if( s ) then
		text = *s + !"\n"
	end if

	text += comment

	tkSetComment( x, text )
end sub

private sub hAccumTkComment( byval x as integer, byval comment as integer )
	assert( tkGet( comment ) = TK_COMMENT )
	hAccumComment( x, *tkGetText( comment ) )
end sub

private sub hStripAllComments( )
	dim as integer x = any
	x = 0
	while( tkGet( x ) <> TK_EOF )
		if( tkGet( x ) = TK_COMMENT ) then
			tkRemove( x, x )
			x -= 1
		end if
		x += 1
	wend
end sub

private function ppComment( byval x as integer ) as integer
	dim as integer y = any, at_bol = any, at_eol = any

	''
	'' int A; //FOO    -> assign FOO to ';', so it can be
	''                    picked up by the A vardecl
	''
	''  /*FOO*/ int A; -> assign FOO to 'int', ditto
	''
	'' //FOO           -> assign FOO to EOL, so it can be
	'' <empty line>       picked up by a TK_DIVIDER
	''
	'' //FOO           -> assign FOO to EOL, ditto
	'' int A;
	'' <empty line>
	''
	'' //FOO           -> comment belongs to both A and B,
	'' int A;             assign to EOL for a TK_DIVIDER
	'' int B;
	''
	'' int /*FOO*/ A;  -> assign FOO to 'int'
	''
	'' int             -> assign FOO to EOL
	'' //FOO
	'' A;

	at_bol = hIsBeforeEol( x, -1 )
	at_eol = hIsBeforeEol( x,  1 )

	if( at_bol and at_eol ) then
		'' Comment above empty line?
		if( tkCount( TK_EOL, x + 1, cSkip( x ) ) >= 2 ) then
			hAccumTkComment( tkSkipSpaceAndComments( x ), x )
		else
			'' Comment above multiple statements,
			'' that aren't separated by empty lines?
			y = cSkipStatement( x )
			if( (y < cSkipStatement( y )) and _
			    (tkCount( TK_EOL, cSkipRev( y ) + 1, y - 1 ) < 2) ) then
				hAccumTkComment( tkSkipSpaceAndComments( x ), x )
			else
				'' Comment above single statement
				hAccumTkComment( cSkip( x ), x )
			end if
		end if
	elseif( at_bol ) then
		hAccumTkComment( tkSkipSpaceAndComments( x ), x )
	elseif( at_eol ) then
		hAccumTkComment( tkSkipSpaceAndComments( x, -1 ), x )
	else
		hAccumTkComment( tkSkipSpaceAndComments( x ), x )
	end if

	tkRemove( x, x )
	x -= 1

	function = x
end function

sub ppComments( )
	dim as integer x = any

	'' TODO
	hStripAllComments( )

	x = 0
	do
		select case( tkGet( x ) )
		case TK_EOF
			exit do

		case TK_COMMENT
			x = ppComment( x )

		end select

		x += 1
	loop
end sub

''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''

'' Merge empty lines into TK_DIVIDER, assuming we're starting at BOL.
private function ppDivider( byval x as integer ) as integer
	dim as integer lines = any, begin = any, eol1 = any, eol2 = any
	dim as string comment, blockcomment

	begin = x

	'' Count empty lines in a row
	lines = 0
	do
		select case( tkGet( x ) )
		case TK_EOL
			lines += 1

		case TK_SPACE

		case else
			exit do
		end select

		x += 1
	loop

	if( lines < 1 ) then
		return -1
	end if

	''  ...code...
	''
	''  //foo
	''
	''  //bar
	''  ...code...
	''
	'' "foo" is the comment associated with TK_DIVIDER, "bar" the one
	'' associated with the following block of code, stored as TK_DIVIDER's
	'' text.

	eol2 = tkSkipSpaceAndComments( x, -1 )
	eol1 = tkSkipSpaceAndComments( eol2 - 1, -1 )
	blockcomment = tkCollectComments( eol1 + 1, eol2 )

	comment = tkCollectComments( begin, eol1 )
	tkRemove( begin, x - 1 )
	tkInsert( begin, TK_DIVIDER, blockcomment )
	tkSetComment( begin, comment )
	x = begin

	function = x
end function

sub ppDividers( )
	dim as integer x = any, old = any

	x = 0
	while( tkGet( x ) <> TK_EOF )
		old = x

		x = ppDivider( old )
		if( x >= 0 ) then
			continue while
		end if

		'' Skip to next BOL
		x = old
		do
			select case( tkGet( x ) )
			case TK_EOF
				exit do
			case TK_EOL
				x += 1
				exit do
			end select

			x += 1
		loop
	wend
end sub
