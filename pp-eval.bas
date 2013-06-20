'' ppEvalExpressions() goes through all #ifs and #elseifs and tries to simplify
'' the expressions. ppAddSymbol() can be used to register symbols as "defined"
'' or "undefined" for #if defined() or #ifdef checks. defined() checks on
'' unknown symbols are not solved out.
''
'' ppEvalIfs() solves out #if/#else blocks if the condition value is known,
'' i.e. expressions that could be simplified down to true or false.

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

const IFSTACKSIZE = 64

type IFSTACKNODE
	cond		as integer '' COND_*
	found_else	as integer
end type

dim shared as IFSTACKNODE ifstack(0 to IFSTACKSIZE-1)

sub ppEvalIfs( )
	dim as integer x = any, level = any, skip = any
	dim as ASTNODE ptr t = any

	x = 0
	level = -1
	skip = -1
	do
		select case( tkGet( x ) )
		case TK_EOF
			exit do

		case TK_AST
			t = tkGetAst( x )

			select case( t->class )
			case ASTCLASS_PPIF, ASTCLASS_PPELSEIF
				if( t->class = ASTCLASS_PPIF ) then
					level += 1
				else
					if( level < 0 ) then
						oops( "#elif without #if" )
					elseif( ifstack(level).found_else ) then
						oops( "#elif after #else" )
					end if
				end if

				if( t->head->class = ASTCLASS_CONST ) then
					assert( typeIsFloat( t->head->dtype ) = FALSE )
					ifstack(level).cond = iif( t->head->val.i <> 0, COND_TRUE, COND_FALSE )

					'' not yet skipping?
					if( skip = -1 ) then
						'' #if FALSE? start skipping
						if( ifstack(level).cond = COND_FALSE ) then
							skip = level + 1
						end if
					end if
				else
					ifstack(level).cond = COND_UNKNOWN
				end if
				ifstack(level).found_else = FALSE

				print level,;
				if( t->class = ASTCLASS_PPIF ) then
					print "if";
				else
					print "elseif";
				end if
				print , "cond=" & ifstack(level).cond, "skip=" & skip

				if( ifstack(level).cond <> COND_UNKNOWN ) then
					tkRemove( x, x )
					t = NULL
					tkInsert( x, TK_EOL )
				end if

			case ASTCLASS_PPELSE
				if( ifstack(level).found_else ) then
					oops( "repeated #else" )
				end if
				ifstack(level).found_else = TRUE

				'' Skipping?
				if( skip >= 0 ) then
					'' Was the #if corresponding to this #else the cause of the skipping?
					if( skip = level + 1 ) then
						'' It must have been an #if FALSE
						assert( ifstack(level).cond = COND_FALSE )

						'' Now stop skipping and parse this #else block
						skip = -1
					end if
				else
					'' Not skipping, so the #if corresponding to this #else must have been
					'' an #if TRUE (and no other skipping from outer levels either).
					assert( ifstack(level).cond = COND_TRUE )

					'' Start skipping now to skip the #else block
					skip = level + 1
				end if

				if( ifstack(level).cond <> COND_UNKNOWN ) then
					tkRemove( x, x )
					t = NULL
					tkInsert( x, TK_EOL )
				end if

				print level, "else", "skip=" & skip

			case ASTCLASS_PPENDIF
				if( level < 0 ) then
					oops( "#endif without #if" )
				end if

				'' Skipping?
				if( skip >= 0 ) then
					'' Was the #if/#else corresponding to this #endif the cause of the skipping?
					if( skip = level + 1 ) then
						'' Now stop skipping
						skip = -1
					end if
				end if

				if( ifstack(level).cond <> COND_UNKNOWN ) then
					tkRemove( x, x )
					t = NULL
					tkInsert( x, TK_EOL )
				end if

				print level, "endif", "skip=" & skip
				level -= 1

			end select

			x += 1

		case else
			if( skip >= 0 ) then
				tkRemove( x, x )
				x -= 1
			end if
			x += 1
		end select
	loop

	if( level >= 0 ) then
		oops( "missing " & level + 1 & " #endif" )
	end if
end sub
