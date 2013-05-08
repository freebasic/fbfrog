'' Token emitter

#include once "fbfrog.bi"

function emitType _
	( _
		byval dtype as integer, _
		byval subtype as ASTNODE ptr _
	) as string

	static as zstring ptr types(0 to TYPE__COUNT-1) = _
	{ _
		NULL       , _
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
		NULL       , _
		NULL         _
	}

	dim as string s
	dim as integer ptrcount = any, dt = any

	dt = typeGetDt( dtype )
	ptrcount = typeGetPtrCount( dtype )

	if( typeIsConstAt( dtype, ptrcount ) ) then
		s += "const "
	end if

	select case( dt )
	case TYPE_UDT, TYPE_PROC
		s += emitAst( subtype )
	case else
		s += *types(dt)
	end select

	for i as integer = (ptrcount - 1) to 0 step -1
		if( typeIsConstAt( dtype, i ) ) then
			s += " const"
		end if

		s += " ptr"
	next

	function = s
end function

private function hIndent( byref s as string ) as string
	function = !"\t" + strReplace( s, !"\n", !"\n\t" )
end function

function emitAst( byval ast as ASTNODE ptr ) as string
	dim as ASTNODE ptr child = any
	dim as string s
	dim as integer count = any

	select case as const( ast->class )
	case ASTCLASS_FILE
		child = ast->childhead
		while( child )
			s += emitAst( child ) + !"\n"
			child = child->next
		wend

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
