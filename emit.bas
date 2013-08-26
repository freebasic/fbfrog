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
		@"integer" , _
		@"uinteger", _
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

private function hIdAndArray( byval n as ASTNODE ptr ) as string
	var s = *n->text
	if( n->array ) then
		s += emitAst( n->array )
	end if
	function = s
end function

private function hCommaList _
	( _
		byval n as ASTNODE ptr, _
		byval spaced as integer _
	) as string

	var s = "("

	var count = 0
	var child = n->head
	while( child )
		if( count > 0 ) then
			s += ", "
		elseif( spaced ) then
			'' space behind '('
			s += " "
		end if

		s += emitAst( child )

		count += 1
		child = child->next
	wend

	if( spaced ) then
		'' space before ')'
		s += " "
	end if

	function = s + ")"
end function

private function emitAst _
	( _
		byval n as ASTNODE ptr, _
		byval need_parens as integer _
	) as string

	dim as string s

	if( n = NULL ) then
		exit function
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

	case ASTCLASS_VERSION
		emitLine( "version" + hCommaList( n->expr, TRUE ) )
		emit.indent += 1
		var child = n->head
		while( child )
			s = emitAst( child )
			child = child->next
		wend
		s = ""
		emit.indent -= 1
		emitLine( "end version" )

	case ASTCLASS_DIVIDER
		emitLine( "" )

	case ASTCLASS_PPINCLUDE
		emitLine( "#include """ + *n->text + """" )

	case ASTCLASS_PPDEFINE
		s += "#define " + *n->text
		if( n->head ) then
			s += hCommaList( n, TRUE )
		end if

		if( n->expr ) then
			s += " "

			assert( n->expr->class = ASTCLASS_MACROBODY )

			var child = n->expr->head
			while( child )

				if( child->attrib and ASTATTRIB_MERGEWITHPREV ) then
					s += "##"
				end if

				select case( child->class )
				case ASTCLASS_MACROPARAM
					if( child->attrib and ASTATTRIB_STRINGIFY ) then
						s += "#"
					end if
					s += *child->text

				case ASTCLASS_TK
					s += tkToCText( child->tk, child->text )

				case else
					assert( FALSE )
				end select

				child = child->next
			wend
		end if

		emitLine( s )
		s = ""

	case ASTCLASS_PPUNDEF
		emitLine( "#undef " + *n->text )

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
		assert( n->array = NULL )
		emitLine( "type " + *n->text + " as " + emitType( n->dtype, n->subtype ) )

	case ASTCLASS_STRUCTFWD
		'' type UDT as UDT_
		'' This way, we only need to translate the <struct UDT { ... }>
		'' body as <type UDT_ : ... : end type>, and everything else
		'' can keep using UDT, that's easier than adjusting all
		'' declarations to use UDT_ in place of UDT.
		emitLine( "type " + *n->text + " as " + *n->text + "_" )

	case ASTCLASS_VAR
		if( n->attrib and ASTATTRIB_EXTERN ) then
			emitLine( "extern "     + hIdAndArray( n ) + " as " + emitType( n->dtype, n->subtype ) )
		elseif( n->attrib and ASTATTRIB_PRIVATE ) then
			emitLine( "dim shared " + hIdAndArray( n ) + " as " + emitType( n->dtype, n->subtype ) )
		else
			emitLine( "extern     " + hIdAndArray( n ) + " as " + emitType( n->dtype, n->subtype ) )
			emitLine( "dim shared " + hIdAndArray( n ) + " as " + emitType( n->dtype, n->subtype ) )
		end if

	case ASTCLASS_FIELD
		emitLine( hIdAndArray( n ) + " as " + emitType( n->dtype, n->subtype ) )

	case ASTCLASS_ENUMCONST
		s += *n->text
		if( n->expr ) then
			s += " = " + emitAst( n->expr )
		end if
		emitLine( s )
		s = ""

	case ASTCLASS_PROC
		assert( n->array = NULL )

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

		if( (n->attrib and ASTATTRIB_HIDECALLCONV) = 0 ) then
			if( n->attrib and ASTATTRIB_CDECL ) then
				assert( (n->attrib and ASTATTRIB_STDCALL) = 0 ) '' can't have both
				s += " cdecl"
			elseif( n->attrib and ASTATTRIB_STDCALL ) then
				s += " stdcall"
			end if
		end if

		s += hCommaList( n, TRUE )

		'' Function result type
		if( n->dtype <> TYPE_ANY ) then
			s += " as " + emitType( n->dtype, n->subtype )
		end if

		if( n->text ) then
			emitLine( s )
			s = ""
		end if

	case ASTCLASS_PARAM
		'' should have been solved out by hFixArrayParams()
		assert( n->array = NULL )

		'' vararg?
		if( n->dtype = TYPE_NONE ) then
			s += "..."
		else
			s += "byval"
			if( n->text ) then
				s += " " + *n->text
			end if
			s += " as " + emitType( n->dtype, n->subtype )

			if( n->expr ) then
				s += " = " + emitAst( n->expr )
			end if
		end if

	case ASTCLASS_ARRAY
		s += hCommaList( n, FALSE )

	case ASTCLASS_DIMENSION
		s += emitAst( n->l )
		s += " to "
		s += emitAst( n->r )

	case ASTCLASS_EXTERNBLOCKBEGIN
		emitLine( "extern """ + *n->text + """" )

	case ASTCLASS_EXTERNBLOCKEND
		emitLine( "end extern" )

	case ASTCLASS_MACROPARAM
		s += *n->text

	case ASTCLASS_CONST
		if( typeIsFloat( n->dtype ) ) then
			s += str( n->valf )
		else
			if( n->attrib and ASTATTRIB_OCT ) then
				s += "&o" + oct( n->vali )
			elseif( n->attrib and ASTATTRIB_HEX ) then
				s += "&h" + hex( n->vali )
			else
				s += str( n->vali )
			end if
		end if

	case ASTCLASS_ID
		s += *n->text

	case ASTCLASS_TEXT
		s += *n->text

	case ASTCLASS_IIF
		s += "iif( " + _
			emitAst( n->expr ) + ", " + _
			emitAst( n->l    ) + ", " + _
			emitAst( n->r    ) + " )"

	case ASTCLASS_BOP
		static as zstring * 8 fbbops(ASTOP_LOGOR to ASTOP_MOD) = _
		{ _
			"orelse" , _  '' ASTOP_LOGOR 
			"andalso", _  '' ASTOP_LOGAND
			"or"     , _  '' ASTOP_BITOR 
			"xor"    , _  '' ASTOP_BITXOR
			"and"    , _  '' ASTOP_BITAND
			"="      , _  '' ASTOP_EQ    
			"<>"     , _  '' ASTOP_NE    
			"<"      , _  '' ASTOP_LT    
			"<="     , _  '' ASTOP_LE    
			">"      , _  '' ASTOP_GT    
			">="     , _  '' ASTOP_GE    
			"shl"    , _  '' ASTOP_SHL   
			"shr"    , _  '' ASTOP_SHR   
			"+"      , _  '' ASTOP_ADD   
			"-"      , _  '' ASTOP_SUB   
			"*"      , _  '' ASTOP_MUL   
			"/"      , _  '' ASTOP_DIV   
			"mod"      _  '' ASTOP_MOD   
		}

		if( need_parens ) then
			s += "("
		end if

		s += emitAst( n->l, TRUE )
		s += " "
		s += fbbops(n->op)
		s += " "
		s += emitAst( n->r, TRUE )

		if( need_parens ) then
			s += ")"
		end if

	case ASTCLASS_UOP
		select case( n->op )
		case ASTOP_LOGNOT
			s += "iif( " + emitAst( n->l ) + ", 0, 1 )"
		case ASTOP_DEFINED
			s += "defined( " + emitAst( n->l ) + " )"
		case else
			if( need_parens ) then
				s += "("
			end if

			select case( n->op )
			case ASTOP_BITNOT    : s += "not "
			case ASTOP_NEGATE    : s += "-"
			case ASTOP_UNARYPLUS : s += "+"
			case else
				assert( FALSE )
			end select

			s += emitAst( n->l, TRUE )

			if( need_parens ) then
				s += ")"
			end if
		end select

	case else
		astDump( n )
		assert( FALSE )
	end select

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
