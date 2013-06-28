'' ppEvalExpressions() goes through all #ifs and #elseifs and tries to simplify
'' the expressions. ppAddSymbol() can be used to register symbols as "defined"
'' or "undefined" for #if defined() or #ifdef checks. defined() checks on
'' unknown symbols are not solved out.
''
'' #if evaluation is done with these 3 steps:
''
'' 1. ppSplitElseIfs(): Splitting up #elseifs blocks into normal
''    #else/#if/#endif blocks
'' 2. ppEvalIfs(): Checks each #if whether the condition is known to be true
''    or false, i.e. expressions that could be simplified down to a constant,
''    and if so, deletes tokens accordingly before continuing to the next #if.
''    Not having to worry about #elseifs greatly simplifies this step.
'' 3. ppMergeElseIfs(): Merging #else/#if/#endif blocks back into #elseifs


#include once "fbfrog.bi"

namespace eval
	dim shared as THASH symbols
end namespace

enum
	COND_UNKNOWN = 0
	COND_TRUE
	COND_FALSE
end enum

sub ppAddSymbol( byval id as zstring ptr, byval is_defined as integer )
	hashAddOverwrite( @eval.symbols, id, cptr( any ptr, is_defined ) )
end sub

sub ppEvalInit( )
	hashInit( @eval.symbols, 4 )
end sub

sub ppEvalEnd( )
	hashEnd( @eval.symbols )
end sub

''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''

private function hLookupSymbol( byval id as zstring ptr ) as integer
	var item = hashLookup( @eval.symbols, id, hashHash( id ) )
	if( item->s ) then
		function = iif( item->data, COND_TRUE, COND_FALSE )
	else
		function = COND_UNKNOWN
	end if
end function

private function hFold( byval n as ASTNODE ptr ) as ASTNODE ptr
	function = n

	if( n = NULL ) then
		exit function
	end if

	var child = n->head
	while( child )
		child = astReplaceChild( n, child, hFold( astClone( child ) ) )
		child = child->next
	wend

	select case as const( n->class )
	case ASTCLASS_DEFINED
		'' defined() on known symbol?
		assert( n->head->class = ASTCLASS_ID )
		var cond = hLookupSymbol( n->head->text )
		if( cond <> COND_UNKNOWN ) then
			'' defined()    ->    1|0
			function = astNewCONST( iif( cond = COND_TRUE, 1, 0 ), 0, TYPE_LONG )
			astDelete( n )
		end if

	case ASTCLASS_IIF
		if( n->head->class = ASTCLASS_CONST ) then
			assert( typeIsFloat( n->dtype ) = FALSE )
			if( n->val.i ) then
				function = astClone( n->head->next )
			else
				function = astClone( n->tail )
			end if
			astDelete( n )
		end if

	case ASTCLASS_LOGOR, ASTCLASS_LOGAND, _
	     ASTCLASS_BITOR, ASTCLASS_BITXOR, ASTCLASS_BITAND, _
	     ASTCLASS_EQ, ASTCLASS_NE, _
	     ASTCLASS_LT, ASTCLASS_LE, _
	     ASTCLASS_GT, ASTCLASS_GE, _
	     ASTCLASS_SHL, ASTCLASS_SHR, _
	     ASTCLASS_ADD, ASTCLASS_SUB, _
	     ASTCLASS_MUL, ASTCLASS_DIV, ASTCLASS_MOD

		if( (n->head->class = ASTCLASS_CONST) and _
		    (n->tail->class = ASTCLASS_CONST) ) then
			assert( typeIsFloat( n->head->dtype ) = FALSE )
			assert( typeIsFloat( n->tail->dtype ) = FALSE )
			var v1 = n->head->val.i
			var v2 = n->tail->val.i

			select case as const( n->class )
			case ASTCLASS_LOGOR  : v1    = iif( v1 orelse  v2, 1, 0 )
			case ASTCLASS_LOGAND : v1    = iif( v1 andalso v2, 1, 0 )
			case ASTCLASS_BITOR  : v1  or= v2
			case ASTCLASS_BITXOR : v1 xor= v2
			case ASTCLASS_BITAND : v1 and= v2
			case ASTCLASS_EQ     : v1    = iif( v1 =  v2, 1, 0 )
			case ASTCLASS_NE     : v1    = iif( v1 <> v2, 1, 0 )
			case ASTCLASS_LT     : v1    = iif( v1 <  v2, 1, 0 )
			case ASTCLASS_LE     : v1    = iif( v1 <= v2, 1, 0 )
			case ASTCLASS_GT     : v1    = iif( v1 >  v2, 1, 0 )
			case ASTCLASS_GE     : v1    = iif( v1 >= v2, 1, 0 )
			case ASTCLASS_SHL    : v1 shl= v2
			case ASTCLASS_SHR    : v1 shr= v2
			case ASTCLASS_ADD    : v1   += v2
			case ASTCLASS_SUB    : v1   -= v2
			case ASTCLASS_MUL    : v1   *= v2
			case ASTCLASS_DIV    : v1   /= v2
			case ASTCLASS_MOD    : v1 mod= v2
			case else
				assert( FALSE )
			end select

			function = astNewCONST( v1, 0, TYPE_LONG )
			astDelete( n )
		end if

	case ASTCLASS_LOGNOT, ASTCLASS_BITNOT, _
	     ASTCLASS_NEGATE, ASTCLASS_UNARYPLUS

		if( n->head->class = ASTCLASS_CONST ) then
			assert( typeIsFloat( n->head->dtype ) = FALSE )
			var v1 = n->head->val.i

			select case as const( n->class )
			case ASTCLASS_LOGNOT    : v1 = iif( v1, 0, 1 )
			case ASTCLASS_BITNOT    : v1 = not v1
			case ASTCLASS_NEGATE    : v1 = -v1
			case ASTCLASS_UNARYPLUS : '' nothing to do
			case else
				assert( FALSE )
			end select

			function = astNewCONST( v1, 0, TYPE_LONG )
			astDelete( n )
		end if

	end select
end function

sub ppEvalExpressions( )
	var x = 0

	while( tkGet( x ) <> TK_EOF )
		if( tkGet( x ) = TK_AST ) then
			var t = tkGetAst( x )

			select case( t->class )
			case ASTCLASS_PPIF, ASTCLASS_PPELSEIF
				t = hFold( astClone( t ) )
				tkSetAst( x, t )
			end select
		end if

		x += 1
	wend
end sub

''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''

'' Find corresponding #else and closing #endif, while stepping over nested
'' #if blocks.
private sub hFindElseEndIf _
	( _
		byval x as integer, _
		byref xelse as integer, _
		byref xendif as integer _
	)

	xelse = -1
	xendif = -1

	var level = 0
	do
		select case( tkGet( x ) )
		case TK_EOF
			exit do

		case TK_AST
			var t = tkGetAst( x )

			select case( t->class )
			case ASTCLASS_PPIF
				level += 1

			case ASTCLASS_PPELSE
				if( level = 0 ) then
					xelse = x
				end if

			case ASTCLASS_PPENDIF
				if( level = 0 ) then
					xendif = x
					exit do
				end if
				level -= 1

			end select

		end select

		x += 1
	loop

	'' If no #else was found, use same position as for #endif
	if( xelse < 0 ) then
		xelse = xendif
	end if
end sub

'' Expand #elseifs into normal #if blocks:
''
'' 1.   #if 1       2. #if 1          3. #if 1
''      #elseif 2      #else             #else
''      #elseif 3          #if 2             #if 2
''      #else              #elseif 3         #else
''      #endif             #else                 #if 3
''                         #endif                #else
''                     #endif                    #endif
''                                           #endif
''                                       #endif
sub ppSplitElseIfs( )
	var x = 0

	do
		select case( tkGet( x ) )
		case TK_EOF
			exit do

		case TK_AST
			var t = tkGetAst( x )

			'' Found an #elseif? Replace it by #else #if
			if( t->class = ASTCLASS_PPELSEIF ) then
				t = astClone( t->head )
				tkRemove( x, x )
				tkInsert( x, TK_AST, , astNew( ASTCLASS_PPELSE ) )
				x += 1
				tkInsert( x, TK_AST, , astNew( ASTCLASS_PPIF, t, NULL, NULL ) )

				'' Find the corresponding #endif,
				'' and insert another #endif in front of it
				dim as integer xelse, xendif
				hFindElseEndIf( x + 1, xelse, xendif )
				tkInsert( xendif, TK_AST, , astNew( ASTCLASS_PPENDIF ) )
			end if

			x += 1

		case else
			x += 1
		end select
	loop
end sub

'' Solve out #if/#else/#endif blocks if the condition is known to be TRUE/FALSE
sub ppEvalIfs( )
	var x = 0
	do
		select case( tkGet( x ) )
		case TK_EOF
			exit do

		case TK_AST
			var t = tkGetAst( x )
			if( t->class = ASTCLASS_PPIF ) then
				var cond = COND_UNKNOWN
				if( t->head->class = ASTCLASS_CONST ) then
					assert( typeIsFloat( t->head->dtype ) = FALSE )
					cond = iif( t->head->val.i <> 0, COND_TRUE, COND_FALSE )
				end if

				if( cond <> COND_UNKNOWN ) then
					dim as integer xelse, xendif
					hFindElseEndIf( x + 1, xelse, xendif )

					if( cond = COND_TRUE ) then
						'' Remove whole #else..#endif block and the #if
						tkRemove( xelse, xendif )
						tkRemove( x, x )
					else
						if( xelse = xendif ) then
							'' No #else found; remove whole #if..#endif block
							tkRemove( x, xendif )
						else
							'' Remove whole #if..#else block and the #endif
							tkRemove( xendif, xendif )
							tkRemove( x, xelse )
						end if
					end if

					x -= 1
				end if
			end if

		end select

		x += 1
	loop
end sub

'' Merge #else #if back into #elseifs
sub ppMergeElseIfs( )
	var x = 0

	do
		select case( tkGet( x ) )
		case TK_EOF
			exit do

		case TK_AST
			'' Found an #else followed by an #if?
			var t = tkGetAst( x )
			if( t->class = ASTCLASS_PPELSE ) then
				if( tkGet( x + 1 ) = TK_AST ) then
					t = tkGetAst( x + 1 )
					if( t->class = ASTCLASS_PPIF ) then
						t = astClone( t->head )
						tkRemove( x, x + 1 )
						tkInsert( x, TK_AST, , astNew( ASTCLASS_PPELSEIF, t, NULL, NULL ) )

						'' Find the corresponding #endif and remove it
						dim as integer xelse, xendif
						hFindElseEndIf( x + 1, xelse, xendif )
						tkRemove( xendif, xendif )
					end if
				end if
			end if

			x += 1

		case else
			x += 1
		end select
	loop
end sub
