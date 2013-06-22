''
'' AST emitter
''
'' emitAst() takes an AST and recursively generates a string containing
'' corresponding formatted FB code. emitWriteFile() is used to create a file
'' from such a string.
''

#include once "fbfrog.bi"

declare function emitAst _
	( _
		byval n as ASTNODE ptr, _
		byval need_parens as integer = FALSE _
	) as string

function emitType _
	( _
		byval dtype as integer, _
		byval subtype as ASTNODE ptr, _
		byval debugdump as integer _
	) as string

	static as zstring ptr datatypenames(0 to TYPE__COUNT-1) = _
	{ _
		@"none"    , _
		@"any"     , _
		@"byte"    , _
		@"ubyte"   , _
		@"zstring" , _
		@"short"   , _
		@"ushort"  , _
		@"long"    , _
		@"ulong"   , _
		@"longint" , _
		@"ulongint", _
		@"single"  , _
		@"double"  , _
		@"udt"     , _
		@"proc"      _
	}

	dim as string s

	var dt = typeGetDt( dtype )
	var ptrcount = typeGetPtrCount( dtype )

	if( typeIsConstAt( dtype, ptrcount ) ) then
		s += "const "
	end if

	if( debugdump ) then
		s += *datatypenames(dt)
	else
		'' If it's a pointer to a function pointer, wrap it inside
		'' a typeof() to prevent the additional PTRs from being seen
		'' as part of the function pointer's result type:
		''    int (**p)(void)
		''    p as function() as integer ptr
		''    p as typeof( function() as integer ) ptr
		'' (alternatively a typedef could be used)
		var add_typeof = (dt = TYPE_PROC) and (ptrcount >= 2)
		if( add_typeof ) then
			s += "typeof( "
		end if

		select case( dt )
		case TYPE_UDT
			s += emitAst( subtype )
		case TYPE_PROC
			if( ptrcount >= 1 ) then
				'' The inner-most PTR on function pointers will be
				'' ignored below, but we still should preserve its CONST
				if( typeIsConstAt( dtype, ptrcount - 1 ) ) then
					s += "const "
				end if
			else
				'' proc type but no pointers -- this is not supported in
				'' place of data types in FB, so here we add a DECLARE to
				'' indicate that it's not supposed to be a procptr type,
				'' but a plain proc type.
				s += "declare "
			end if
			s += emitAst( subtype )
		case else
			s += *datatypenames(dt)
		end select

		if( add_typeof ) then
			s += " )"
		end if

		'' Ignore most-inner PTR on function pointers -- in FB it's already
		'' implied by writing AS SUB|FUNCTION( ... ).
		if( dt = TYPE_PROC ) then
			'assert( ptrcount > 0 )
			if( ptrcount >= 1 ) then
				ptrcount -= 1
			end if
		end if
	end if

	for i as integer = (ptrcount - 1) to 0 step -1
		if( typeIsConstAt( dtype, i ) ) then
			s += " const"
		end if

		s += " ptr"
	next

	function = s
end function

namespace emit
	dim shared as integer indent, fo
end namespace

private sub emitLine( byref ln as string )
	var s = trim( ln, any !" \t\n\r" )

	if( len( s ) > 0 ) then
		for i as integer = 1 to emit.indent
			print #emit.fo, !"\t";
		next
	end if

	print #emit.fo, s
end sub

private function emitAst _
	( _
		byval n as ASTNODE ptr, _
		byval need_parens as integer _
	) as string

	dim as string s

	if( n = NULL ) then
		exit function
	end if

	if( n->attrib and ASTATTRIB_PPINDENTEND ) then
		emit.indent -= 1
	end if

	select case as const( n->class )
	case ASTCLASS_NOP

	case ASTCLASS_GROUP
		var child = n->head
		while( child )
			s = emitAst( child )
			child = child->next
		wend
		s = ""

	case ASTCLASS_DIVIDER
		emitLine( "" )

	case ASTCLASS_PPINCLUDE
		emitLine( "#include """ + *n->text + """" )

	case ASTCLASS_PPDEFINE
		s += "#define " + *n->text
		if( n->head ) then
			s += " " + emitAst( n->head )
		end if
		emitLine( s )
		s = ""

	case ASTCLASS_PPIF
		select case( n->head->class )
		'' #if defined id     ->    #ifdef id
		case ASTCLASS_DEFINED
			s += "#ifdef " + emitAst( n->head->head )

		'' #if !defined id    ->    #ifndef id
		case ASTCLASS_LOGNOT
			if( n->head->head->class = ASTCLASS_DEFINED ) then
				s += "#ifndef " + emitAst( n->head->head->head )
			end if
		end select

		if( len( s ) = 0 ) then
			s += "#if " + emitAst( n->head )
		end if

		emitLine( s )
		s = ""

	case ASTCLASS_PPELSEIF
		emitLine( "#elseif " + emitAst( n->head ) )

	case ASTCLASS_PPELSE
		emitLine( "#else" )

	case ASTCLASS_PPENDIF
		emitLine( "#endif" )

	case ASTCLASS_PPUNKNOWN
		emitLine( "'' TODO: unknown PP directive" )
		emitLine( emitAst( n->head ) )

	case ASTCLASS_STRUCT, ASTCLASS_UNION, ASTCLASS_ENUM
		dim as string compoundkeyword
		select case( n->class )
		case ASTCLASS_UNION
			compoundkeyword = "union"
		case ASTCLASS_ENUM
			compoundkeyword = "enum"
		case else
			compoundkeyword = "type"
		end select

		s += compoundkeyword
		if( n->text ) then
			s += " " + *n->text
		end if
		emitLine( s )
		s = ""
		emit.indent += 1

		var child = n->head
		while( child )
			s = emitAst( child )
			child = child->next
		wend
		s = ""

		emit.indent -= 1
		emitLine( "end " + compoundkeyword )

	case ASTCLASS_TYPEDEF
		emitLine( "type " + *n->text + " as " + emitType( n->dtype, n->subtype ) )

	case ASTCLASS_VAR
		if( n->attrib and ASTATTRIB_EXTERN ) then
			emitLine( "extern "     + *n->text + " as " + emitType( n->dtype, n->subtype ) )
		elseif( n->attrib and ASTATTRIB_STATIC ) then
			emitLine( "dim shared " + *n->text + " as " + emitType( n->dtype, n->subtype ) )
		else
			emitLine( "extern     " + *n->text + " as " + emitType( n->dtype, n->subtype ) )
			emitLine( "dim shared " + *n->text + " as " + emitType( n->dtype, n->subtype ) )
		end if

	case ASTCLASS_FIELD
		emitLine( *n->text + " as " + emitType( n->dtype, n->subtype ) )

	case ASTCLASS_ENUMCONST
		s += *n->text
		if( n->head ) then
			s += " = " + emitAst( n->head )
		end if
		emitLine( s )
		s = ""

	case ASTCLASS_PROC
		'' Is this a procedure declaration,
		'' or the subtype of a procedure pointer?
		if( n->text ) then
			s += "declare "
		end if

		if( n->dtype = TYPE_ANY ) then
			s += "sub"
		else
			s += "function"
		end if

		if( n->text ) then
			s += " " + *n->text
		end if

		s += "("

		var count = 0
		var child = n->head
		while( child )
			if( count > 0 ) then
				s += ","
			end if
			s += " "

			s += emitAst( child )

			count += 1
			child = child->next
		wend

		s += " )"

		'' Function result type
		if( n->dtype <> TYPE_ANY ) then
			s += " as " + emitType( n->dtype, n->subtype )
		end if

		if( n->text ) then
			emitLine( s )
			s = ""
		end if

	case ASTCLASS_PARAM
		'' vararg?
		if( n->dtype = TYPE_NONE ) then
			s += "..."
		else
			s += "byval"
			if( n->text ) then
				s += " " + *n->text
			end if
			s += " as " + emitType( n->dtype, n->subtype )
		end if

	case ASTCLASS_UNKNOWN
		emitLine( "'' TODO: unknown construct" )
		emitLine( emitAst( n->head ) )

	case ASTCLASS_CONST
		if( typeIsFloat( n->dtype ) ) then
			s += str( n->val.f )
		else
			if( n->attrib and ASTATTRIB_OCT ) then
				s += "&o" + oct( n->val.i )
			elseif( n->attrib and ASTATTRIB_HEX ) then
				s += "&h" + hex( n->val.i )
			else
				s += str( n->val.i )
			end if
		end if
	case ASTCLASS_ID
		s += *n->text
	case ASTCLASS_TEXT
		s += *n->text

	case ASTCLASS_DEFINED
		s += "defined( " + emitAst( n->head ) + " )"

	case ASTCLASS_IIF
		s += "iif( " + _
			emitAst( n->head       ) + ", " + _
			emitAst( n->head->next ) + ", " + _
			emitAst( n->tail       ) + " )"

	case ASTCLASS_LOGOR, ASTCLASS_LOGAND, _
	     ASTCLASS_BITOR, ASTCLASS_BITXOR, ASTCLASS_BITAND, _
	     ASTCLASS_EQ, ASTCLASS_NE, _
	     ASTCLASS_LT, ASTCLASS_LE, _
	     ASTCLASS_GT, ASTCLASS_GE, _
	     ASTCLASS_SHL, ASTCLASS_SHR, _
	     ASTCLASS_ADD, ASTCLASS_SUB, _
	     ASTCLASS_MUL, ASTCLASS_DIV, ASTCLASS_MOD

		if( need_parens ) then
			s += "("
		end if

		s += emitAst( n->head, TRUE )
		s += " "

		select case as const( n->class )
		case ASTCLASS_LOGOR  : s += "orelse"
		case ASTCLASS_LOGAND : s += "andalso"
		case ASTCLASS_BITOR  : s += "or"
		case ASTCLASS_BITXOR : s += "xor"
		case ASTCLASS_BITAND : s += "and"
		case ASTCLASS_EQ     : s += "="
		case ASTCLASS_NE     : s += "<>"
		case ASTCLASS_LT     : s += "<"
		case ASTCLASS_LE     : s += "<="
		case ASTCLASS_GT     : s += ">"
		case ASTCLASS_GE     : s += ">="
		case ASTCLASS_SHL    : s += "shl"
		case ASTCLASS_SHR    : s += "shr"
		case ASTCLASS_ADD    : s += "+"
		case ASTCLASS_SUB    : s += "-"
		case ASTCLASS_MUL    : s += "*"
		case ASTCLASS_DIV    : s += "/"
		case ASTCLASS_MOD    : s += "mod"
		case else
			assert( FALSE )
		end select

		s += " "
		s += emitAst( n->tail, TRUE )

		if( need_parens ) then
			s += ")"
		end if

	case ASTCLASS_LOGNOT
		s += "iif( " + emitAst( n->head, FALSE ) + ", 0, 1 )"

	case ASTCLASS_BITNOT, ASTCLASS_NEGATE, ASTCLASS_UNARYPLUS
		if( need_parens ) then
			s += "("
		end if

		select case as const( n->class )
		case ASTCLASS_BITNOT
			s += "not "
		case ASTCLASS_NEGATE
			s += "-"
		case ASTCLASS_UNARYPLUS
			s += "+"
		case else
			assert( FALSE )
		end select

		s += emitAst( n->head, TRUE )

		if( need_parens ) then
			s += ")"
		end if

	case else
		assert( FALSE )
	end select

	if( n->attrib and ASTATTRIB_PPINDENTBEGIN ) then
		emit.indent += 1
	end if

	function = s
end function

sub emitFile( byref filename as string, byval ast as ASTNODE ptr )
	emit.indent = 0
	emit.fo = freefile( )
	if( open( filename, for output, as #emit.fo ) ) then
		oops( "could not open output file: '" + filename + "'" )
	end if

	var s = emitAst( ast )

	close #emit.fo
end sub
