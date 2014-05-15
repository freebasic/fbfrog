'' C/CPP expression folding

#include once "fbfrog.bi"

private function astExprContains _
	( _
		byval n as ASTNODE ptr, _
		byval nodeclass as integer _
	) as integer

	if( n = NULL ) then return FALSE
	if( n->class = nodeclass ) then return TRUE

	function = astExprContains( n->expr, nodeclass ) or _
		astExprContains( n->l, nodeclass ) or _
		astExprContains( n->r, nodeclass )
end function

private function astOpC2FB _
	( _
		byval n as ASTNODE ptr, _
		byval fbop as integer _
	) as ASTNODE ptr
	n->class = fbop
	function = astNewUOP( ASTCLASS_NEGATE, n )
end function

function astOpsC2FB( byval n as ASTNODE ptr ) as ASTNODE ptr
	if( n = NULL ) then exit function

	n->expr = astOpsC2FB( n->expr )
	n->l = astOpsC2FB( n->l )
	n->r = astOpsC2FB( n->r )

	select case( n->class )
	case ASTCLASS_CLOGNOT
		'' !x    =    x == 0
		n->r = astNewCONSTI( 0, TYPE_LONG )
		n = astOpC2FB( n, ASTCLASS_EQ )
	case ASTCLASS_CDEFINED : n = astOpC2FB( n, ASTCLASS_DEFINED )
	case ASTCLASS_CLOGOR   : n = astOpC2FB( n, ASTCLASS_ORELSE )
	case ASTCLASS_CLOGAND  : n = astOpC2FB( n, ASTCLASS_ANDALSO )
	case ASTCLASS_CEQ      : n = astOpC2FB( n, ASTCLASS_EQ )
	case ASTCLASS_CNE      : n = astOpC2FB( n, ASTCLASS_NE )
	case ASTCLASS_CLT      : n = astOpC2FB( n, ASTCLASS_LT )
	case ASTCLASS_CLE      : n = astOpC2FB( n, ASTCLASS_LE )
	case ASTCLASS_CGT      : n = astOpC2FB( n, ASTCLASS_GT )
	case ASTCLASS_CGE      : n = astOpC2FB( n, ASTCLASS_GE )
	end select

	function = n
end function

private function hKeepAttribs _
	( _
		byval n as ASTNODE ptr, _
		byval l as ASTNODE ptr, _
		byval r as ASTNODE ptr = NULL _
	) as ASTNODE ptr

	var rattrib = iif( r, r->attrib, 0 )

	const OCTHEX = ASTATTRIB_OCT or ASTATTRIB_HEX

	'' If one operand is oct/hex, use that for the result too.
	'' If both are decimal, keep using decimal for the result.
	n->attrib or= (l->attrib or rattrib) and OCTHEX

	'' If operands had different oct/hex status, default to hex.
	if( (n->attrib and OCTHEX) = OCTHEX ) then
		n->attrib and= not ASTATTRIB_OCT
	end if

	function = n
end function

private function astFoldConsts( byval n as ASTNODE ptr, byref changes as integer ) as ASTNODE ptr
	if( n = NULL ) then exit function
	var t = n

	n->expr = astFoldConsts( n->expr, changes )
	n->l = astFoldConsts( n->l, changes )
	n->r = astFoldConsts( n->r, changes )

	select case( n->class )
	case ASTCLASS_NOT, ASTCLASS_NEGATE
		if( astIsCONSTI( n->l ) ) then
			select case( n->class )
			case ASTCLASS_NOT       : t = hKeepAttribs( astNewCONSTI( not n->l->vali, TYPE_LONG ), n->l )
			case ASTCLASS_NEGATE    : t = hKeepAttribs( astNewCONSTI( -n->l->vali, TYPE_LONG ), n->l )
			end select
		end if

	case ASTCLASS_ORELSE, ASTCLASS_ANDALSO, _
	     ASTCLASS_OR, ASTCLASS_XOR, ASTCLASS_AND, _
	     ASTCLASS_EQ, ASTCLASS_NE, _
	     ASTCLASS_LT, ASTCLASS_LE, _
	     ASTCLASS_GT, ASTCLASS_GE, _
	     ASTCLASS_SHL, ASTCLASS_SHR, _
	     ASTCLASS_ADD, ASTCLASS_SUB, _
	     ASTCLASS_MUL, ASTCLASS_DIV, ASTCLASS_MOD
		if( astIsCONSTI( n->l ) and astIsCONSTI( n->r ) ) then
			select case( n->class )
			case ASTCLASS_ORELSE  : t =               astNewCONSTI( n->l->vali orelse  n->r->vali, TYPE_LONG )
			case ASTCLASS_ANDALSO : t =               astNewCONSTI( n->l->vali andalso n->r->vali, TYPE_LONG )
			case ASTCLASS_OR      : t = hKeepAttribs( astNewCONSTI( n->l->vali or      n->r->vali, TYPE_LONG ), n->l, n->r )
			case ASTCLASS_XOR     : t = hKeepAttribs( astNewCONSTI( n->l->vali xor     n->r->vali, TYPE_LONG ), n->l, n->r )
			case ASTCLASS_AND     : t = hKeepAttribs( astNewCONSTI( n->l->vali and     n->r->vali, TYPE_LONG ), n->l, n->r )
			case ASTCLASS_EQ      : t =               astNewCONSTI( n->l->vali =       n->r->vali, TYPE_LONG )
			case ASTCLASS_NE      : t =               astNewCONSTI( n->l->vali <>      n->r->vali, TYPE_LONG )
			case ASTCLASS_LT      : t =               astNewCONSTI( n->l->vali <       n->r->vali, TYPE_LONG )
			case ASTCLASS_LE      : t =               astNewCONSTI( n->l->vali <=      n->r->vali, TYPE_LONG )
			case ASTCLASS_GT      : t =               astNewCONSTI( n->l->vali >       n->r->vali, TYPE_LONG )
			case ASTCLASS_GE      : t =               astNewCONSTI( n->l->vali >=      n->r->vali, TYPE_LONG )
			case ASTCLASS_SHL     : t = hKeepAttribs( astNewCONSTI( n->l->vali shl     n->r->vali, TYPE_LONG ), n->l, n->r )
			case ASTCLASS_SHR     : t = hKeepAttribs( astNewCONSTI( n->l->vali shr     n->r->vali, TYPE_LONG ), n->l, n->r )
			case ASTCLASS_ADD     : t = hKeepAttribs( astNewCONSTI( n->l->vali +       n->r->vali, TYPE_LONG ), n->l, n->r )
			case ASTCLASS_SUB     : t = hKeepAttribs( astNewCONSTI( n->l->vali -       n->r->vali, TYPE_LONG ), n->l, n->r )
			case ASTCLASS_MUL     : t = hKeepAttribs( astNewCONSTI( n->l->vali *       n->r->vali, TYPE_LONG ), n->l, n->r )
			case ASTCLASS_DIV
				'' Not divison by zero?
				if( n->r->vali <> 0 ) then
					t = hKeepAttribs( astNewCONSTI( n->l->vali \ n->r->vali, TYPE_LONG ), n->l, n->r )
				end if
			case ASTCLASS_MOD
				'' Not divison by zero?
				if( n->r->vali <> 0 ) then
					t = hKeepAttribs( astNewCONSTI( n->l->vali mod n->r->vali, TYPE_LONG ), n->l, n->r )
				end if
			end select
		end if

	case ASTCLASS_IIF
		'' Constant condition?
		if( astIsCONSTI( n->expr ) ) then
			'' iif( true , l, r ) = l
			'' iif( false, l, r ) = r
			if( n->expr->vali ) then
				t = n->l : n->l = NULL
			else
				t = n->r : n->r = NULL
			end if
		end if
	end select

	if( t <> n ) then
		astDelete( n )
		changes += 1
	end if
	function = t
end function

'' For commutative BOPs where only the lhs is a CONSTI, swap lhs/rhs so the
'' CONSTI ends up on the rhs on as many BOPs as possible (not on all, because
'' not all are commutative). That simplifies some checks for BOPs with only
'' one CONSTI operand, because only the rhs needs to be checked.
private sub astSwapConstsToRhs( byval n as ASTNODE ptr, byref changes as integer )
	if( n = NULL ) then exit sub

	astSwapConstsToRhs( n->expr, changes )
	astSwapConstsToRhs( n->l, changes )
	astSwapConstsToRhs( n->r, changes )

	select case( n->class )
	case ASTCLASS_ADD, ASTCLASS_MUL, ASTCLASS_AND, ASTCLASS_OR, _
	     ASTCLASS_XOR, ASTCLASS_EQ, ASTCLASS_NE, _
	     ASTCLASS_LT, ASTCLASS_LE, ASTCLASS_GT, ASTCLASS_GE, _
	     ASTCLASS_SUB
		'' Only the lhs is a CONSTI?
		if( astIsCONSTI( n->l ) and (not astIsCONSTI( n->r )) ) then
			'' Lhs/rhs of commutative BOPs can just be swapped:
			'' N and x   =   x and N
			'' N or  x   =   x or  N
			'' N xor x   =   x xor N
			'' N =   x   =   x =   N
			'' N <>  x   =   x <>  N
			'' N +   x   =   x +   N
			'' N *   x   =   x *   N

			'' For some other BOPs we can something similar:
			select case( n->class )
			'' N <  x   =   x >  N
			'' N <= x   =   x >= N
			'' N >  x   =   x <  N
			'' N >= x   =   x <= N
			case ASTCLASS_LT : n->class = ASTCLASS_GT
			case ASTCLASS_LE : n->class = ASTCLASS_GE
			case ASTCLASS_GT : n->class = ASTCLASS_LT
			case ASTCLASS_GE : n->class = ASTCLASS_LE
			'' N - x   =   N + -x   =   -x + N
			case ASTCLASS_SUB : n->class = ASTCLASS_ADD : n->r = astNewUOP( ASTCLASS_NEGATE, n->r )
			end select

			swap n->l, n->r
			changes += 1
		end if
	end select
end sub

private function hIsFreeOfSideFx( byval n as ASTNODE ptr ) as integer
	function = not astExprContains( n, ASTCLASS_CALL )
end function

private function hIsBoolOp( byval n as ASTNODE ptr ) as integer
	select case( n->class )
	case ASTCLASS_EQ, ASTCLASS_NE, ASTCLASS_LT, _
	     ASTCLASS_LE, ASTCLASS_GT, ASTCLASS_GE, _
	     ASTCLASS_ORELSE, ASTCLASS_ANDALSO, ASTCLASS_DEFINED
		function = TRUE
	end select
end function

private function astFoldNops( byval n as ASTNODE ptr, byref changes as integer ) as ASTNODE ptr
	if( n = NULL ) then exit function
	var t = n

	n->expr = astFoldNops( n->expr, changes )
	n->l = astFoldNops( n->l, changes )
	n->r = astFoldNops( n->r, changes )

	select case( n->class )
	'' +x = x
	case ASTCLASS_UNARYPLUS
		t = n->l : n->l = NULL

	case ASTCLASS_ORELSE
		'' true  orelse x   = -1
		'' false orelse x   = x
		if( astIsCONSTI( n->l ) ) then
			if( n->l->vali ) then
				t = astNewCONSTI( -1, TYPE_LONG )
			else
				t = n->r : n->r = NULL
			end if

		'' x orelse true    = -1    unless sidefx
		'' x orelse false   = x
		elseif( astIsCONSTI( n->r ) ) then
			if( n->r->vali ) then
				if( hIsFreeOfSideFx( n->l ) ) then
					t = astNewCONSTI( -1, TYPE_LONG )
				end if
			else
				t = n->l : n->l = NULL
			end if
		end if

	case ASTCLASS_ANDALSO
		'' true  andalso x   = x
		'' false andalso x   = 0
		if( astIsCONSTI( n->l ) and (not astIsCONSTI( n->r )) ) then
			if( n->l->vali ) then
				t = n->r : n->r = NULL
			else
				t = astNewCONSTI( 0, TYPE_LONG )
			end if

		'' x andalso true    = x
		'' x andalso false   = 0    unless sidefx
		elseif( (not astIsCONSTI( n->l )) and astIsCONSTI( n->r ) ) then
			if( n->r->vali ) then
				t = n->l : n->l = NULL
			else
				if( hIsFreeOfSideFx( n->l ) ) then
					t = astNewCONSTI( 0, TYPE_LONG )
				end if
			end if
		end if

	'' x or  0 =  x
	'' x or -1 = -1    unless sidefx
	case ASTCLASS_OR
		if( (not astIsCONSTI( n->l )) and astIsCONSTI( n->r ) ) then
			select case( n->r->vali )
			case 0
				t = n->l : n->l = NULL
			case -1
				if( hIsFreeOfSideFx( n->l ) ) then
					t = hKeepAttribs( astNewCONSTI( -1, TYPE_LONG ), n->r )
				end if
			end select
		end if

	'' x and 0 = 0    unless sidefx
	case ASTCLASS_AND
		if( (not astIsCONSTI( n->l )) and astIsCONSTI( n->r ) ) then
			if( (n->r->vali = 0) and hIsFreeOfSideFx( n->l ) ) then
				t = hKeepAttribs( astNewCONSTI( 0, TYPE_LONG ), n->r )
			end if
		end if

	'' bool =  0    =    not bool
	'' bool = -1    =    bool
	case ASTCLASS_EQ
		if( hIsBoolOp( n->l ) and astIsCONSTI( n->r ) ) then
			select case( n->r->vali )
			case 0
				'' Delete rhs and turn = BOP into not UOP
				astDelete( n->r ) : n->r = NULL
				n->class = ASTCLASS_NOT
				changes += 1
			case -1
				t = n->l : n->l = NULL
			end select
		end if

	'' bool <>  0   =    bool
	'' bool <> -1   =    not bool
	case ASTCLASS_NE
		if( hIsBoolOp( n->l ) and astIsCONSTI( n->r ) ) then
			select case( n->r->vali )
			case 0
				t = n->l : n->l = NULL
			case -1
				'' Delete rhs and turn <> BOP into not UOP
				astDelete( n->r ) : n->r = NULL
				n->class = ASTCLASS_NOT
				changes += 1
			end select
		end if

	case ASTCLASS_SHL, ASTCLASS_SHR
		'' 0 shl x = 0    unless sidefx
		'' 0 shr x = 0    unless sidefx
		if( astIsCONSTI( n->l ) ) then
			if( (n->l->vali = 0) and hIsFreeOfSideFx( n->r ) ) then
				t = hKeepAttribs( astNewCONSTI( 0, TYPE_LONG ), n->l )
			end if

		'' x shl 0 =  x
		'' x shr 0 =  x
		elseif( astIsCONSTI( n->r ) ) then
			if( n->r->vali = 0 ) then
				t = n->l : n->l = NULL
			end if
		end if

	'' x + 0 = x
	'' x - 0 = x
	case ASTCLASS_ADD, ASTCLASS_SUB
		if( astIsCONSTI( n->r ) ) then
			if( n->r->vali = 0 ) then
				t = n->l : n->l = NULL
			end if
		end if

	'' x * 0 = 0    unless sidefx
	'' x * 1 = x
	case ASTCLASS_MUL
		if( astIsCONSTI( n->r ) ) then
			select case( n->r->vali )
			case 0
				if( hIsFreeOfSideFx( n->l ) ) then
					t = hKeepAttribs( astNewCONSTI( 0, TYPE_LONG ), n->r )
				end if
			case 1
				t = n->l : n->l = NULL
			end select
		end if

	'' 0 /   x = 0    if x <> 0 and no sidefx
	'' 0 mod x = 0    if x <> 0 and no sidefx
	case ASTCLASS_DIV, ASTCLASS_MOD
		if( astIsCONSTI( n->l ) ) then
			if( (n->l->vali = 0) and hIsFreeOfSideFx( n->r ) and _
			    iif( astIsCONSTI( n->r ), n->r->vali <> 0, TRUE ) ) then
				t = hKeepAttribs( astNewCONSTI( 0, TYPE_LONG ), n->l )
			end if
		end if

	case ASTCLASS_IIF
		'' Same true/false expressions?
		'' iif( condition, X, X ) = X    unless sidefx in condition
		if( astIsEqual( n->l, n->r ) and hIsFreeOfSideFx( n->expr ) ) then
			t = n->l : n->l = NULL
		end if
	end select

	if( t <> n ) then
		astDelete( n )
		changes += 1
	end if
	function = t
end function

private function astFoldNestedOps( byval n as ASTNODE ptr, byref changes as integer ) as ASTNODE ptr
	if( n = NULL ) then exit function
	var t = n

	n->expr = astFoldNestedOps( n->expr, changes )
	n->l = astFoldNestedOps( n->l, changes )
	n->r = astFoldNestedOps( n->r, changes )

	select case( n->class )
	'' not (not x) = x
	'' not (ll relbop lr) = ll inverse_relbop lr
	case ASTCLASS_NOT
		select case( n->l->class )
		case ASTCLASS_NOT : t = n->l->l : n->l->l = NULL
		case ASTCLASS_EQ : n->l->class = ASTCLASS_NE : t = n->l : n->l = NULL '' not =   ->  <>
		case ASTCLASS_NE : n->l->class = ASTCLASS_EQ : t = n->l : n->l = NULL '' not <>  ->  =
		case ASTCLASS_LT : n->l->class = ASTCLASS_GE : t = n->l : n->l = NULL '' not <   ->  >=
		case ASTCLASS_LE : n->l->class = ASTCLASS_GT : t = n->l : n->l = NULL '' not <=  ->  >
		case ASTCLASS_GT : n->l->class = ASTCLASS_LE : t = n->l : n->l = NULL '' not >   ->  <=
		case ASTCLASS_GE : n->l->class = ASTCLASS_LT : t = n->l : n->l = NULL '' not >=  ->  <
		end select

	'' -(-x) = x
	case ASTCLASS_NEGATE
		if( n->l->class = ASTCLASS_NEGATE ) then
			t = n->l->l : n->l->l = NULL
		end if

	'' (-bool) = 1    =    bool = -1
	case ASTCLASS_EQ
		if( (n->l->class = ASTCLASS_NEGATE) and astIsCONSTI( n->r ) ) then
			if( hIsBoolOp( n->l->l ) and (n->r->vali = 1) ) then
				'' On the lhs, replace the - UOP by its own operand
				var negatenode = n->l
				n->l = negatenode->l : negatenode->l = NULL
				astDelete( negatenode )
				'' On the rhs, turn 1 into -1
				n->r->vali = -1
				changes += 1
			end if
		end if
	end select

	if( t <> n ) then
		astDelete( n )
		changes += 1
	end if
	function = t
end function

private function astFoldBoolContextNops _
	( _
		byval n as ASTNODE ptr, _
		byval is_bool_context as integer, _
		byref changes as integer _
	) as ASTNODE ptr

	if( n = NULL ) then exit function
	function = n

	var expr_is_bool_context = FALSE
	var l_is_bool_context = FALSE
	var r_is_bool_context = FALSE

	select case( n->class )
	'' BOP operands may sometimes be in bool context:
	''    x = 0
	''    x <> 0
	'' (not x = -1 and x <> -1 because if checks check against 0 and <> 0,
	'' not 0 and -1)
	case ASTCLASS_EQ, ASTCLASS_NE
		if( astIsCONSTI( n->r ) ) then
			l_is_bool_context = (n->r->vali = 0)
		end if

	'' andalso/orelse operands are always treated as bools
	case ASTCLASS_ORELSE, ASTCLASS_ANDALSO
		l_is_bool_context = TRUE
		r_is_bool_context = TRUE

	'' iif() condition always is treated as bool
	case ASTCLASS_IIF
		expr_is_bool_context = TRUE
	end select

	n->expr = astFoldBoolContextNops( n->expr, expr_is_bool_context, changes )
	n->l = astFoldBoolContextNops( n->l, l_is_bool_context, changes )
	n->r = astFoldBoolContextNops( n->r, r_is_bool_context, changes )

	if( is_bool_context ) then
		select case( n->class )
		'' Negation in bool context is a no-op:
		'' if( -x ) then   =   if( x ) then
		case ASTCLASS_NEGATE
			function = n->l : n->l = NULL
			astDelete( n )
			changes += 1
		end select
	end if
end function

function astFold _
	( _
		byval n as ASTNODE ptr, _
		byval is_bool_context as integer _
	) as ASTNODE ptr

	'' Loop until the folding no longer changes the expression
	dim as integer changes
	do
		changes = 0
		n = astFoldConsts( n, changes )
		astSwapConstsToRhs( n, changes )
		n = astFoldNops( n, changes )
		n = astFoldNestedOps( n, changes )
		n = astFoldBoolContextNops( n, is_bool_context, changes )
	loop while( changes > 0 )

	function = n
end function
