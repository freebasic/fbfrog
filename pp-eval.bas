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

namespace fi
	const IFSTACKSIZE = 64

	type IFSTACKNODE
		cond		as integer '' COND_*
		found_else	as integer
	end type

	dim shared as IFSTACKNODE stack(0 to IFSTACKSIZE-1)
	dim shared as integer skip, level, x
end namespace

private sub hStartSkip( )
	'' not yet skipping?
	if( fi.skip = -1 ) then
		'' #if FALSE, or #else of #if TRUE? start skipping
		with( fi.stack(fi.level) )
			if( ((not .found_else) and (.cond = COND_FALSE)) or _
			    (     .found_else  and (.cond = COND_TRUE )) ) then
				fi.skip = fi.level + 1
			end if
		end with
	end if
end sub

private sub hStopSkip( )
	'' Skipping at current level?
	if( fi.skip = fi.level + 1 ) then
		'' Now stop skipping
		fi.skip = -1
	end if
end sub

private sub hCheckCond( byval t as ASTNODE ptr )
	if( t->head->class = ASTCLASS_CONST ) then
		assert( typeIsFloat( t->head->dtype ) = FALSE )
		fi.stack(fi.level).cond = iif( t->head->val.i <> 0, COND_TRUE, COND_FALSE )
	else
		fi.stack(fi.level).cond = COND_UNKNOWN
	end if
	fi.stack(fi.level).found_else = FALSE
end sub

private sub hDeleteToken( )
	if( fi.stack(fi.level).cond <> COND_UNKNOWN ) then
		tkRemove( fi.x, fi.x )
		tkInsert( fi.x, TK_EOL )
	end if
end sub

sub ppEvalIfs( )
	fi.x = 0
	fi.level = -1
	fi.skip = -1
	do
		select case( tkGet( fi.x ) )
		case TK_EOF
			exit do

		case TK_AST
			var t = tkGetAst( fi.x )

			select case( t->class )
			case ASTCLASS_PPIF, ASTCLASS_PPELSEIF
				if( t->class = ASTCLASS_PPIF ) then
					fi.level += 1
				else
					if( fi.level < 0 ) then
						oops( "#elif without #if" )
					elseif( fi.stack(fi.level).found_else ) then
						oops( "#elif after #else" )
					end if

					hStopSkip( )
				end if

				hCheckCond( t )
				hStartSkip( )

				print fi.level,;
				if( t->class = ASTCLASS_PPIF ) then
					print "if";
				else
					print "elseif";
				end if
				print , "cond=" & fi.stack(fi.level).cond, "skip=" & fi.skip

				hDeleteToken( )

			case ASTCLASS_PPELSE
				if( fi.stack(fi.level).found_else ) then
					oops( "repeated #else" )
				end if
				fi.stack(fi.level).found_else = TRUE

				hStopSkip( )
				hStartSkip( )
				hDeleteToken( )
				print fi.level, "else", "skip=" & fi.skip

			case ASTCLASS_PPENDIF
				if( fi.level < 0 ) then
					oops( "#endif without #if" )
				end if

				hStopSkip( )
				hDeleteToken( )

				print fi.level, "endif", "skip=" & fi.skip
				fi.level -= 1

			end select

			fi.x += 1

		case else
			if( fi.skip >= 0 ) then
				tkRemove( fi.x, fi.x )
				fi.x -= 1
			end if
			fi.x += 1
		end select
	loop

	if( fi.level >= 0 ) then
		oops( "missing " & fi.level + 1 & " #endif" )
	end if
end sub
