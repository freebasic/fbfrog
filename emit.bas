'' Token emitter

#include once "fbfrog.bi"

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
	dim as integer ptrcount = any, dt = any, add_typeof = any

	dt = typeGetDt( dtype )
	ptrcount = typeGetPtrCount( dtype )

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
		add_typeof = (dt = TYPE_PROC) and (ptrcount >= 2)
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

private function hIndent( byref s as string ) as string
	'' Not just newlines?
	if( len( strReplace( s, !"\n", "" ) ) > 0 ) then
		function = !"\t" + strReplace( s, !"\n", !"\n\t" )
	end if
end function

function emitAst( byval ast as ASTNODE ptr ) as string
	dim as ASTNODE ptr child = any
	dim as string s
	dim as integer count = any

	select case as const( ast->class )
	case ASTCLASS_NOP

	case ASTCLASS_GROUP
		child = ast->childhead
		while( child )
			s += emitAst( child )
			child = child->next
			if( child ) then
				s += !"\n"
			end if
		wend

	case ASTCLASS_DIVIDER

	case ASTCLASS_PPINCLUDE
		s += "#include """ + *ast->text + """"
	case ASTCLASS_PPDEFINE
		s += "#define " + *ast->id + " " + *ast->text
	case ASTCLASS_PPIF
		s += "#if " + emitAst( ast->l )
	case ASTCLASS_PPELSE
		s += "#else"
	case ASTCLASS_PPENDIF
		s += "#endif"
	case ASTCLASS_PPUNKNOWN
		s += "'' TODO: unknown PP directive" + !"\n"
		s += *ast->text

	case ASTCLASS_STRUCT
		s += "type " + *ast->id + !"\n"

		child = ast->childhead
		while( child )
			s += hIndent( emitAst( child ) ) + !"\n"
			child = child->next
		wend

		s += "end type"

	case ASTCLASS_TYPEDEF
		s += "type " + *ast->id + " as " + emitType( ast->dtype, ast->subtype )

	case ASTCLASS_VAR
		if( ast->attrib and ASTATTRIB_EXTERN ) then
			s += "extern "     + *ast->id + " as " + emitType( ast->dtype, ast->subtype )
		elseif( ast->attrib and ASTATTRIB_STATIC ) then
			s += "dim shared " + *ast->id + " as " + emitType( ast->dtype, ast->subtype )
		else
			s += "extern     " + *ast->id + " as " + emitType( ast->dtype, ast->subtype ) + !"\n"
			s += "dim shared " + *ast->id + " as " + emitType( ast->dtype, ast->subtype )
		end if

	case ASTCLASS_FIELD
		s += *ast->id + " as " + emitType( ast->dtype, ast->subtype )

	case ASTCLASS_PROC
		'' Is this a procedure declaration,
		'' or the subtype of a procedure pointer?
		if( ast->id ) then
			s += "declare "
		end if

		if( ast->dtype = TYPE_ANY ) then
			s += "sub"
		else
			s += "function"
		end if

		if( ast->id ) then
			s += " " + *ast->id
		end if

		s += "("

		count = 0
		child = ast->childhead
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
		if( ast->dtype <> TYPE_ANY ) then
			s += " as " + emitType( ast->dtype, ast->subtype )
		end if

	case ASTCLASS_PARAM
		'' vararg?
		if( ast->dtype = TYPE_NONE ) then
			s += "..."
		else
			s += "byval"
			if( ast->id ) then
				s += " " + *ast->id
			end if
			s += " as " + emitType( ast->dtype, ast->subtype )
		end if

	case ASTCLASS_UNKNOWN
		s += "'' TODO: unknown construct" + !"\n"
		s += *ast->text

	case ASTCLASS_CONST
		s += str( ast->intval )

	case ASTCLASS_ID
		s += *ast->id

	case ASTCLASS_DEFINED
		s += "defined( " + emitAst( ast->l ) + " )"

	case ASTCLASS_LOGICNOT
		s += "(" + emitAst( ast->l ) + ") = 0"

	case else
		assert( FALSE )
	end select

	function = s
end function

sub emitWriteFile( byref filename as string, byref text as string )
	dim as integer fo = any

	fo = freefile( )
	if( open( filename, for output, as #fo ) ) then
		oops( "could not open output file: '" + filename + "'" )
	end if

	print #fo, text

	close #fo
end sub
