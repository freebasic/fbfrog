''
'' AST emitter
''
'' emitAst() takes an AST and recursively generates a string containing
'' corresponding formatted FB code. emitWriteFile() is used to create a file
'' from such a string.
''

#include once "fbfrog.bi"

declare sub emitStmt( byref s as string, byval comment as zstring ptr = NULL )

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
		@"short"   , _
		@"ushort"  , _
		@"long"    , _
		@"ulong"   , _
		@"clong"   , _
		@"culong"  , _
		@"integer" , _
		@"uinteger", _
		@"longint" , _
		@"ulongint", _
		@"single"  , _
		@"double"  , _
		@"udt"     , _
		@"proc"    , _
		@"zstring" , _
		@"wstring"   _
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

private sub emitEmptyLine( )
	print #emit.fo, ""
end sub

private sub emitIndentedLine( byref ln as string )
	assert( len( ln ) > 0 )

	dim s as string

	for i as integer = 1 to emit.indent
		s += !"\t"
	next

	s += ln
	print #emit.fo, s
end sub

private function hPrependSpace( byref comment as string ) as string
	var s = comment

	'' Prepend a space if none is there yet
	if( left( s, 1 ) <> " " ) then
		s = " " + s
	end if

	function = s
end function

private sub emitLineComment( byref comment as string )
	emitIndentedLine( "''" + hPrependSpace( comment ) )
end sub

private sub hFlushLineComment( byref ln as string )
	if( len( ln ) > 0 ) then
		emitLineComment( ln )
		ln = ""
	end if
end sub

'' The comment text may contain newlines, but we need to emit every line
'' individually to be able to prepend it with the ' and indentation.
private sub emitLineComments( byval comment as zstring ptr )
	dim ln as string
	for i as integer = 0 to len( *comment )-1
		select case( (*comment)[i] )
		case CH_LF, CH_CR
			'' Flush previous line, ignore this newline, and then
			'' continue with next line
			hFlushLineComment( ln )
		case else
			ln += chr( (*comment)[i] )
		end select
	next
	hFlushLineComment( ln )
end sub

private function hIsMultiLineComment( byval comment as zstring ptr ) as integer
	do
		select case( (*comment)[0] )
		case 0 : exit do
		case CH_LF, CH_CR : return TRUE
		end select
		comment += 1
	loop
	function = FALSE
end function

private sub emitStmt( byref s as string, byval comment as zstring ptr )
	assert( len( s ) > 0 )
	if( comment ) then
		if( hIsMultiLineComment( comment ) ) then
			emitLineComments( comment )
			emitIndentedLine( s )
		else
			emitIndentedLine( s + "  ''" + hPrependSpace( *comment ) )
		end if
	else
		emitIndentedLine( s )
	end if
end sub

private function hIdAndArray( byval n as ASTNODE ptr ) as string
	var s = *n->text
	if( n->array ) then
		s += emitAst( n->array )
	end if
	function = s
end function

private function hSeparatedList _
	( _
		byval n as ASTNODE ptr, _
		byval separator as zstring ptr, _
		byval need_parens as integer _
	) as string

	dim s as string

	var count = 0
	var child = n->head
	while( child )
		if( count > 0 ) then
			s += *separator
		end if

		s += emitAst( child, need_parens )

		count += 1
		child = child->next
	wend

	function = s
end function

private function hParamList( byval n as ASTNODE ptr ) as string
	function = "(" + hSeparatedList( n, ", ", FALSE ) + ")"
end function

private sub hEmitIndentedChildren( byval n as ASTNODE ptr )
	emit.indent += 1
	var i = n->head
	while( i )
		var s = emitAst( i )
		if( len( s ) > 0 ) then
			emitStmt( s )
		end if
		i = i->next
	wend
	emit.indent -= 1
end sub

function emitAst _
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

	case ASTCLASS_VERBLOCK
		emitStmt( "version " + hSeparatedList( n->expr, ", ", FALSE ) )
		hEmitIndentedChildren( n )
		emitStmt( "endversion" )

	case ASTCLASS_TARGETBLOCK
		if( n->attrib and ASTATTRIB_DOS   ) then s += "dos, "
		if( n->attrib and ASTATTRIB_LINUX ) then s += "linux, "
		if( n->attrib and ASTATTRIB_WIN32 ) then s += "win32, "

		'' Cut off ", " at the end
		s = left( s, len( s ) - 2 )

		emitStmt( "target " + s )
		s = ""

		hEmitIndentedChildren( n )

		emitStmt( "endtarget" )

	case ASTCLASS_DIVIDER
		if( (n->prev <> NULL) and _
		    ((n->next <> NULL) or _
		     (n->comment <> NULL) or _
		     (n->text <> NULL)) ) then
			emitEmptyLine( )
		end if

		if( n->comment ) then
			emitLineComments( n->comment )
			if( n->next ) then
				emitEmptyLine( )
			end if
		end if

		if( n->text ) then
			emitLineComments( n->text )
		end if

	case ASTCLASS_SCOPEBLOCK
		emitStmt( "scope" )
		hEmitIndentedChildren( n )
		emitStmt( "end scope" )

	case ASTCLASS_PPINCLUDE
		emitStmt( "#include """ + *n->text + """", n->comment )

	case ASTCLASS_PPDEFINE
		if( n->expr andalso (n->expr->class = ASTCLASS_SCOPEBLOCK) ) then
			s += "#macro " + *n->text
			if( n->head ) then
				s += hParamList( n )
			end if
			emitStmt( s, n->comment )
			s = ""

			emit.indent += 1
			s = emitAst( n->expr )
			emit.indent -= 1

			emitStmt( "#endmacro" )
		else
			s += "#define " + *n->text
			if( n->head ) then
				s += hParamList( n )
			end if

			if( n->expr ) then
				s += " " + emitAst( n->expr, TRUE )
			end if

			emitStmt( s, n->comment )
			s = ""
		end if

	case ASTCLASS_PPUNDEF
		emitStmt( "#undef " + *n->text, n->comment )

	case ASTCLASS_PPIF
		if( n->expr->class = ASTCLASS_UOP ) then
			select case( n->expr->op )
			'' #if defined( id )        ->    #ifdef id
			case ASTOP_DEFINED
				s = "#ifdef " + emitAst( n->expr->l )

			'' #if not defined( id )    ->    #ifndef id
			case ASTOP_NOT
				if( n->expr->l->class = ASTCLASS_UOP ) then
					if( n->expr->l->op = ASTOP_DEFINED ) then
						s = "#ifndef " + emitAst( n->expr->l->l )
					end if
				end if
			end select
		end if
		if( len( s ) = 0 ) then
			s = "#if " + emitAst( n->expr )
		end if
		emitStmt( s )
		s = ""

		hEmitIndentedChildren( n )

	case ASTCLASS_PPELSEIF
		emitStmt( "#elseif " + emitAst( n->expr ) )
		hEmitIndentedChildren( n )

	case ASTCLASS_PPELSE
		emitStmt( "#else" )
		hEmitIndentedChildren( n )

	case ASTCLASS_PPENDIF
		emitStmt( "#endif" )

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
		emitStmt( s, n->comment )
		s = ""
		emit.indent += 1

		var child = n->head
		while( child )
			s = emitAst( child )
			child = child->next
		wend
		s = ""

		emit.indent -= 1
		emitStmt( "end " + compoundkeyword )

	case ASTCLASS_TYPEDEF
		assert( n->array = NULL )
		emitStmt( "type " + *n->text + " as " + emitType( n->dtype, n->subtype ), n->comment )

	case ASTCLASS_STRUCTFWD
		'' type UDT as UDT_
		'' This way, we only need to translate the <struct UDT { ... }>
		'' body as <type UDT_ : ... : end type>, and everything else
		'' can keep using UDT, that's easier than adjusting all
		'' declarations to use UDT_ in place of UDT.
		emitStmt( "type " + *n->text + " as " + *n->text + "_", n->comment )

	case ASTCLASS_VAR
		if( n->attrib and ASTATTRIB_EXTERN ) then
			emitStmt( "extern "     + hIdAndArray( n ) + " as " + emitType( n->dtype, n->subtype ), n->comment )
		elseif( n->attrib and ASTATTRIB_PRIVATE ) then
			emitStmt( "dim shared " + hIdAndArray( n ) + " as " + emitType( n->dtype, n->subtype ), n->comment )
		else
			emitStmt( "extern     " + hIdAndArray( n ) + " as " + emitType( n->dtype, n->subtype ), n->comment )
			emitStmt( "dim shared " + hIdAndArray( n ) + " as " + emitType( n->dtype, n->subtype ), n->comment )
		end if

	case ASTCLASS_FIELD
		emitStmt( hIdAndArray( n ) + " as " + emitType( n->dtype, n->subtype ), n->comment )

	case ASTCLASS_ENUMCONST
		s += *n->text
		if( n->expr ) then
			s += " = " + emitAst( n->expr )
		end if
		emitStmt( s, n->comment )
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

		s += hParamList( n )

		'' Function result type
		if( n->dtype <> TYPE_ANY ) then
			s += " as " + emitType( n->dtype, n->subtype )
		end if

		if( n->text ) then
			emitStmt( s, n->comment )
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
		s += hParamList( n )

	case ASTCLASS_DIMENSION
		if( n->l ) then
			assert( n->r )
			s += emitAst( n->l ) + " to " + emitAst( n->r )
		else
			assert( n->r = NULL )
			s += "0 to ..."
		end if

	case ASTCLASS_EXTERNBLOCKBEGIN
		emitStmt( "extern """ + *n->text + """" )

	case ASTCLASS_EXTERNBLOCKEND
		emitStmt( "end extern" )

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
		s += *n->text + emitAst( n->expr )

	case ASTCLASS_STRING, ASTCLASS_CHAR
		s = """"

		'' Turn the string literal from the internal format into
		'' something nice for FB code
		var has_escapes = FALSE
		dim as ubyte ptr i = n->text
		do
			select case( i[0] )
			case 0
				exit do

			'' Internal format: can contain \\ and \0 escape
			'' sequences to encode embedded null chars
			case CH_BACKSLASH
				i += 1
				if( i[0] = CH_0 ) then
					'' If a digit is following, then ensure it doesn't
					'' become part of this escape sequence
					'' (FB's \NNN decimal escape sequence is limited to 3 chars)
					select case( i[1] )
					case CH_0 to CH_9
						s += $"\000"
					case else
						s += $"\0"
					end select
				else
					assert( i[0] = CH_BACKSLASH )
					s += $"\\"
				end if
				has_escapes = TRUE

			case CH_BELL      : s += $"\a"  : has_escapes = TRUE
			case CH_BACKSPACE : s += $"\b"  : has_escapes = TRUE
			case CH_TAB       : s += $"\t"  : has_escapes = TRUE
			case CH_LF        : s += $"\n"  : has_escapes = TRUE
			case CH_VTAB      : s += $"\v"  : has_escapes = TRUE
			case CH_FORMFEED  : s += $"\f"  : has_escapes = TRUE
			case CH_CR        : s += $"\r"  : has_escapes = TRUE
			case CH_DQUOTE    : s += $"\""" : has_escapes = TRUE

			case is < 32, is >= 127
				var n = str( i[0] )

				'' If a digit is following, then ensure it doesn't
				'' become part of this escape sequence
				'' (FB's \NNN decimal escape sequence is limited to 3 chars)
				select case( i[1] )
				case CH_0 to CH_9
					n = string( 3 - len( n ), "0" ) + n
				end select
				s += $"\" + n
				has_escapes = TRUE

			case else
				s += chr( i[0] )
			end select

			i += 1
		loop

		s += """"

		if( has_escapes ) then
			s = "!" + s
		end if

		if( typeGetDtAndPtr( n->dtype ) = TYPE_WSTRING ) then
			s = "wstr( " + s + " )"
		end if

		if( n->class = ASTCLASS_CHAR ) then
			s = "asc( " + s + " )"
		end if

	case ASTCLASS_UOP
		select case as const( n->op )
		'case ASTOP_CLOGNOT
		'	s = "iif( " + emitAst( n->l ) + ", 0, 1 )"
		'	need_parens = FALSE
		case ASTOP_NOT       : s = "not " + emitAst( n->l, TRUE )
		case ASTOP_NEGATE    : s = "-"    + emitAst( n->l, TRUE )
		case ASTOP_UNARYPLUS : s = "+"    + emitAst( n->l, TRUE )
		'case ASTOP_CDEFINED
		'	s = "-defined( " + emitAst( n->l ) + " )"
		case ASTOP_DEFINED
			s = "defined( " + emitAst( n->l ) + " )"
			need_parens = FALSE
		case ASTOP_ADDROF    : s = "@"    + emitAst( n->l, TRUE )
		case ASTOP_DEREF     : s = "*"    + emitAst( n->l, TRUE )
		case ASTOP_STRINGIFY : s = "#"    + emitAst( n->l ) : need_parens = FALSE
		case ASTOP_SIZEOF    : s = "sizeof( " + emitAst( n->l ) + " )" : need_parens = FALSE
		case ASTOP_CAST
			select case( n->dtype )
			case TYPE_BYTE     : s =    "cbyte("
			case TYPE_UBYTE    : s =   "cubyte("
			case TYPE_SHORT    : s =   "cshort("
			case TYPE_USHORT   : s =  "cushort("
			case TYPE_LONG     : s =     "clng("
			case TYPE_ULONG    : s =    "culng("
			case TYPE_INTEGER  : s =     "cint("
			case TYPE_UINTEGER : s =    "cuint("
			case TYPE_LONGINT  : s =  "clngint("
			case TYPE_ULONGINT : s = "culngint("
			case else
				if( typeGetPtrCount( n->dtype ) > 0 ) then
					s = "cptr("
				else
					s = "cast("
				end if
				s += emitType( n->dtype, n->subtype ) + ", "
			end select

			s += emitAst( n->l ) + ")"
			need_parens = FALSE

		case else
			assert( FALSE )
		end select

		if( need_parens ) then s = "(" + s + ")"

	case ASTCLASS_BOP
		#define lhs emitAst( n->l, TRUE )
		#define rhs emitAst( n->r, TRUE )

		select case as const( n->op )
		'case ASTOP_CLOGOR      : s = "-(" + lhs + " orelse "  + rhs + ")"
		'case ASTOP_CLOGAND     : s = "-(" + lhs + " andalso " + rhs + ")"
		case ASTOP_ORELSE      : s =        lhs + " orelse "  + rhs
		case ASTOP_ANDALSO     : s =        lhs + " andalso " + rhs
		case ASTOP_OR          : s =        lhs + " or "      + rhs
		case ASTOP_XOR         : s =        lhs + " xor "     + rhs
		case ASTOP_AND         : s =        lhs + " and "     + rhs
		'case ASTOP_CEQ         : s = "-(" + lhs + " = "       + rhs + ")"
		'case ASTOP_CNE         : s = "-(" + lhs + " <> "      + rhs + ")"
		'case ASTOP_CLT         : s = "-(" + lhs + " < "       + rhs + ")"
		'case ASTOP_CLE         : s = "-(" + lhs + " <= "      + rhs + ")"
		'case ASTOP_CGT         : s = "-(" + lhs + " > "       + rhs + ")"
		'case ASTOP_CGE         : s = "-(" + lhs + " >= "      + rhs + ")"
		case ASTOP_EQ          : s =        lhs + " = "       + rhs
		case ASTOP_NE          : s =        lhs + " <> "      + rhs
		case ASTOP_LT          : s =        lhs + " < "       + rhs
		case ASTOP_LE          : s =        lhs + " <= "      + rhs
		case ASTOP_GT          : s =        lhs + " > "       + rhs
		case ASTOP_GE          : s =        lhs + " >= "      + rhs
		case ASTOP_SHL         : s =        lhs + " shl "     + rhs
		case ASTOP_SHR         : s =        lhs + " shr "     + rhs
		case ASTOP_ADD         : s =        lhs + " + "       + rhs
		case ASTOP_SUB         : s =        lhs + " - "       + rhs
		case ASTOP_MUL         : s =        lhs + " * "       + rhs
		case ASTOP_DIV         : s =        lhs + " / "       + rhs
		case ASTOP_MOD         : s =        lhs + " mod "     + rhs
		case ASTOP_INDEX       : s =        lhs + "["         + rhs + "]" : need_parens = FALSE
		case ASTOP_MEMBER      : s =        lhs + "."         + rhs : need_parens = FALSE
		case ASTOP_MEMBERDEREF : s =        lhs + "->"        + rhs : need_parens = FALSE
		case ASTOP_STRCAT      : s =        lhs + " + "       + rhs
		case else
			assert( FALSE )
		end select

		if( need_parens ) then s = "(" + s + ")"

	case ASTCLASS_IIF
		s += "iif( " + _
			emitAst( n->expr ) + ", " + _
			emitAst( n->l    ) + ", " + _
			emitAst( n->r    ) + " )"

	case ASTCLASS_PPMERGE
		var child = n->head
		while( child )
			if( child <> n->head ) then s += "##"
			s += emitAst( child )
			child = child->next
		wend

	case ASTCLASS_CALL
		s = *n->text + hParamList( n )

	case ASTCLASS_STRUCTINIT
		s = hParamList( n )

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
