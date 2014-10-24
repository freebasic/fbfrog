''
'' "AST to FB .bi file" emitter
''

#include once "fbfrog.bi"

#define emitL( n ) emitAst( (n)->head, TRUE )
#define emitR( n ) emitAst( (n)->tail, TRUE )

declare function emitAst _
	( _
		byval n as ASTNODE ptr, _
		byval need_parens as integer = FALSE, _
		byval parentclass as integer = -1 _
	) as string

function emitType overload _
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
		@"clongdouble", _
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
			s += "typeof("
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
			if( (dt = TYPE_ZSTRING) and (subtype <> NULL) ) then
				s += " * " + emitAst( subtype )
			end if
		end select

		if( add_typeof ) then
			s += ")"
		end if

		'' Ignore most-inner PTR on function pointers -- in FB it's already
		'' implied by writing AS SUB|FUNCTION( ... ).
		if( dt = TYPE_PROC ) then
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

function emitType overload( byval n as ASTNODE ptr ) as string
	function = emitType( n->dtype, n->subtype )
end function

namespace emit
	dim shared as integer fo, indent, comment, commentspaces
end namespace

private sub emitLine( byref ln as string )
	dim s as string

	'' Only add indentation if the line will contain more than that
	if( (len( ln ) > 0) or (emit.comment > 0) ) then
		s += string( emit.indent, !"\t" )
	end if

	if( emit.comment > 0 ) then
		s += "''"
		if( len( ln ) > 0 ) then
			s += space( emit.commentspaces )
		end if
	end if

	s += ln
	print #emit.fo, s
end sub

private sub hFlushLine( byval begin as zstring ptr, byval p as ubyte ptr )
	if( begin > p ) then
		exit sub
	end if

	'' Insert a null terminator at EOL temporarily, so the current line
	'' text can be read from the string directly, instead of having to be
	'' copied out char-by-char.
	dim as integer old = p[0]
	p[0] = 0
	emitLine( *begin )
	p[0] = old
end sub

'' Given text that contains newlines, emit every line individually and prepend
'' indentation and/or <'' > if it's a comment.
private sub emitLines( byval lines as zstring ptr )
	dim as ubyte ptr p = lines
	dim as zstring ptr begin = p

	do
		select case( p[0] )
		case 0
			'' EOF not behind EOL? Treat as EOL.
			hFlushLine( begin, p )
			exit do

		case CH_LF, CH_CR
			hFlushLine( begin, p )

			'' CRLF?
			if( (p[0] = CH_CR) and (p[1] = CH_LF) ) then
				p += 1
			end if

			p += 1

			'' EOF after EOL? Don't emit another empty line.
			if( p[0] = 0 ) then
				exit do
			end if

			begin = p

		case else
			p += 1
		end select
	loop
end sub

'' Symbols can have an explicit alias set due to symbol renaming.
'' Normally though we rely on astWrapInExternBlock() to add the Extern block,
'' which makes it unnecessary to worry about emitting case-preserving aliases.
private function hEmitAlias( byval n as ASTNODE ptr ) as string
	if( n->alias ) then
		function = " alias """ + *n->alias + """"
	end if
end function

private function hIdAndArray( byval n as ASTNODE ptr, byval allow_alias as integer ) as string
	var s = *n->text
	if( allow_alias ) then
		s += hEmitAlias( n )
	end if
	if( n->array ) then
		s += emitAst( n->array )
	end if
	if( n->bits ) then
		s += " : " + emitAst( n->bits )
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
	var i = n->head
	while( i )
		if( count > 0 ) then
			s += *separator
		end if

		s += emitAst( i, need_parens )

		count += 1
		i = i->next
	wend

	function = s
end function

private function hParamList( byval n as ASTNODE ptr ) as string
	function = "(" + hSeparatedList( n, ", ", FALSE ) + ")"
end function

private sub hEmitIndentedChildren( byval n as ASTNODE ptr, byval parentclass as integer = -1 )
	if( emit.comment > 0 ) then
		emit.commentspaces += 4
	else
		emit.indent += 1
	end if

	var i = n->head
	while( i )
		var s = emitAst( i, , parentclass )
		if( len( s ) > 0 ) then
			emitLine( s )
		end if
		i = i->next
	wend

	if( emit.comment > 0 ) then
		emit.commentspaces -= 4
	else
		emit.indent -= 1
	end if
end sub

private function hParens( byref s as string, byval need_parens as integer ) as string
	if( need_parens ) then
		s = "(" + s + ")"
	end if
	function = s
end function

private function hInitializer( byval n as ASTNODE ptr ) as string
	if( n->expr ) then
		function = " = " + emitAst( n->expr )
	end if
end function

private sub emitVarDecl( byref prefix as string, byval n as ASTNODE ptr, byval is_extern as integer )
	var s = prefix

	if( is_extern ) then
		if( ((n->attrib and ASTATTRIB_EXTERN) <> 0) and _
		    ((n->attrib and ASTATTRIB_DLLIMPORT) <> 0) ) then
			s += "import "
		end if
	end if

	s += hIdAndArray( n, is_extern ) + " as " + emitType( n )
	if( is_extern = FALSE ) then s += hInitializer( n )

	emitLine( s )
end sub

private function emitAst _
	( _
		byval n as ASTNODE ptr, _
		byval need_parens as integer, _
		byval parentclass as integer _
	) as string

	dim as string s

	if( n = NULL ) then
		exit function
	end if

	select case as const( n->class )
	case ASTCLASS_GROUP
		var i = n->head
		while( i )
			s = emitAst( i )
			s = ""
			i = i->next
		wend

	case ASTCLASS_DIVIDER
		if( (n->prev <> NULL) and (n->next <> NULL) ) then
			emitLine( "" )
		end if

	case ASTCLASS_VEROR, ASTCLASS_VERAND
		var i = n->head
		while( i )
			s += emitAst( i, TRUE )
			if( i->next ) then
				if( astIsVEROR( n ) ) then
					s += " or "
				else
					s += " and "
				end if
			end if
			i = i->next
		wend
		s = hParens( s, need_parens )

	case ASTCLASS_SCOPEBLOCK
		emitLine( "scope" )
		hEmitIndentedChildren( n )
		emitLine( "end scope" )

	case ASTCLASS_UNKNOWN
		assert( n->expr->class = ASTCLASS_TEXT )
		emit.comment += 1
		emit.commentspaces += 1
		emitLine( "TODO: unrecognized construct:" )
		emitLines( n->expr->text )
		emitLine( string( 75, "-" ) )
		emitLines( n->text )
		emit.commentspaces -= 1
		emit.comment -= 1

	case ASTCLASS_RENAMELIST
		var added_indent = FALSE
		if( emit.comment = 0 ) then
			added_indent = TRUE
			emit.comment += 1
			emit.commentspaces += 1
		end if

		emitLine( *n->text )

		hEmitIndentedChildren( n )

		if( added_indent ) then
			emit.commentspaces -= 1
			emit.comment -= 1
		end if

	case ASTCLASS_INCLIB
		emitLine( "#inclib """ + *n->text + """" )

	case ASTCLASS_PRAGMAONCE
		emitLine( "#pragma once" )

	case ASTCLASS_PPINCLUDE
		emitLine( "#include once """ + *n->text + """" )

	case ASTCLASS_PPDEFINE
		if( n->expr andalso (n->expr->class = ASTCLASS_SCOPEBLOCK) ) then
			s += "#macro " + *n->text
			if( n->head ) then
				s += hParamList( n )
			end if
			emitLine( s )
			s = ""

			emit.indent += 1
			s = emitAst( n->expr )
			s = ""
			emit.indent -= 1

			emitLine( "#endmacro" )
		else
			s += "#define " + *n->text
			if( n->paramcount >= 0 ) then
				s += hParamList( n )
			end if

			if( n->expr ) then
				s += " " + emitAst( n->expr, TRUE )
			end if

			emitLine( s )
			s = ""
		end if

	case ASTCLASS_PPIF
		select case( n->expr->class )
		'' #if defined(id)        ->    #ifdef id
		case ASTCLASS_DEFINED
			s = "#ifdef " + emitAst( n->expr->head )

		'' #if not defined(id)    ->    #ifndef id
		case ASTCLASS_NOT
			if( n->expr->head->class = ASTCLASS_DEFINED ) then
				s = "#ifndef " + emitAst( n->expr->head->head )
			end if
		end select
		if( len( s ) = 0 ) then
			s = "#if " + emitAst( n->expr )
		end if
		emitLine( s )
		s = ""

		hEmitIndentedChildren( n )

	case ASTCLASS_PPELSEIF
		emitLine( "#elseif " + emitAst( n->expr ) )
		hEmitIndentedChildren( n )

	case ASTCLASS_PPELSE
		emitLine( "#else" )
		hEmitIndentedChildren( n )

	case ASTCLASS_PPENDIF
		emitLine( "#endif" )

	case ASTCLASS_PPERROR
		emitLine( "#error " + emitAst( n->expr ) )

	case ASTCLASS_STRUCT, ASTCLASS_UNION, ASTCLASS_ENUM
		if( (n->class = ASTCLASS_ENUM) and (n->text <> NULL) ) then
			emitLine( "type " + *n->text + " as long" )
		end if

		'' If it's a struct inside a struct, or union inside union,
		'' insert a union/struct in between respectively, to make it
		'' FB-compatible. FB only allows alternating types/unions when
		'' nesting.
		var opposite = iif( n->class = ASTCLASS_STRUCT, "union", "type" )
		if( n->class = parentclass ) then
			assert( parentclass <> ASTCLASS_ENUM )
			emitLine( opposite )
			emit.indent += 1
		end if

		dim as string compound
		select case( n->class )
		case ASTCLASS_UNION : compound = "union"
		case ASTCLASS_ENUM  : compound = "enum"
		case else           : compound = "type"
		end select

		s += compound
		if( (n->class <> ASTCLASS_ENUM) and (n->text <> NULL) ) then
			s += " " + *n->text
		end if
		if( n->attrib and ASTATTRIB_PACKED ) then
			s += " field = 1"
		elseif( n->maxalign > 0 ) then
			s += " field = " & n->maxalign
		end if
		emitLine( s )
		s = ""

		hEmitIndentedChildren( n, n->class )

		emitLine( "end " + compound )

		if( n->class = parentclass ) then
			emit.indent -= 1
			emitLine( "end " + opposite )
		end if

	case ASTCLASS_TYPEDEF
		assert( n->array = NULL )
		emitLine( "type " + *n->text + " as " + emitType( n ) )

	case ASTCLASS_ENUMCONST
		emitLine( *n->text + hInitializer( n ) )

	case ASTCLASS_VAR
		if( n->attrib and ASTATTRIB_LOCAL ) then
			if( n->attrib and ASTATTRIB_STATIC ) then
				emitVarDecl( "static ", n, FALSE )
			else
				emitVarDecl( "dim ", n, FALSE )
			end if
		else
			if( n->attrib and ASTATTRIB_EXTERN ) then
				emitVarDecl( "extern ", n, TRUE )
			elseif( n->attrib and ASTATTRIB_STATIC ) then
				emitVarDecl( "dim shared ", n, FALSE )
			else
				emitVarDecl( "extern     ", n, TRUE )
				emitVarDecl( "dim shared ", n, FALSE )
			end if
		end if

	case ASTCLASS_FIELD
		'' Fields can be named after keywords, but we have to do
		''     as type foo
		'' instead of
		''     foo as type
		'' if foo has special meaning at the beginning of a statement in
		'' a TYPE block.
		var use_multdecl = FALSE

		select case( lcase( *n->text, 1 ) )
		case "as", "static", "dim", "redim", "const", "declare", _
		     "end", "type", "union", "enum", "rem", _
		     "public", "private", "protected"
			use_multdecl = TRUE
		end select

		if( use_multdecl ) then
			emitLine( "as " + emitType( n ) + " " + hIdAndArray( n, FALSE ) )
		else
			emitLine( hIdAndArray( n, FALSE ) + " as " + emitType( n ) )
		end if

	case ASTCLASS_PROC
		assert( n->array = NULL )

		var compound = iif( n->dtype = TYPE_ANY, "sub", "function" )

		'' Declaration and not a procptr subtype or body?
		if( (n->text <> NULL) and (n->expr = NULL) ) then
			s += "declare "
		end if

		'' SUB|FUNCTION [<id>]
		s += compound
		if( n->text ) then
			s += " " + *n->text + hEmitAlias( n )
		end if

		'' Calling convention not covered by Extern block?
		if( (n->attrib and ASTATTRIB_HIDECALLCONV) = 0 ) then
			if( n->attrib and ASTATTRIB_CDECL ) then
				assert( (n->attrib and ASTATTRIB_STDCALL) = 0 ) '' can't have both
				s += " cdecl"
			elseif( n->attrib and ASTATTRIB_STDCALL ) then
				s += " stdcall"
			end if
		end if

		'' '(' Parameters... ')'
		s += hParamList( n )

		'' Function result type
		if( n->dtype <> TYPE_ANY ) then
			s += " as " + emitType( n )
		end if

		'' Is this a procedure declaration/statement (and not a procptr subtype)?
		if( n->text ) then
			emitLine( s )
			s = ""

			'' Body
			if( n->expr ) then
				assert( n->expr->class = ASTCLASS_GROUP )
				hEmitIndentedChildren( n->expr )
				emitLine( "end " + compound )
			end if
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
			s += " as " + emitType( n )
			s += hInitializer( n )
		end if

	case ASTCLASS_ARRAY
		s += hParamList( n )

	case ASTCLASS_EXTERNBLOCKBEGIN
		emitLine( "extern """ + *n->text + """" )

	case ASTCLASS_EXTERNBLOCKEND
		emitLine( "end extern" )

	case ASTCLASS_MACROPARAM
		s += *n->text
		if( n->attrib and ASTATTRIB_VARIADIC ) then
			s += "..."
		end if

	case ASTCLASS_CONSTI, ASTCLASS_CONSTF
		dim as string suffix
		var need_rparen = FALSE

		select case( typeGetDtAndPtr( n->dtype ) )
		case TYPE_CLONG
			s += "cast(clong, "
			need_rparen = TRUE
		case TYPE_CULONG
			s += "cast(culong, "
			need_rparen = TRUE
		case TYPE_LONGINT
			suffix = "ll"
		case TYPE_ULONGINT
			suffix = "ull"

		''
		'' Float type suffixes:
		''
		'' FB defaults to DOUBLE as long as there is a fractional part,
		'' so no type suffix is needed for that, as in C.
		''
		'' Without a fractional part a suffix would be more useful,
		'' turning the literal into a float instead of integer,
		'' but that probably does not make much of a difference,
		'' since FB will do the conversion silently anyways.
		''
		'' The f suffix should be added for SINGLEs though, to ensure
		'' it's using the intended precision.
		''
		case TYPE_SINGLE
			suffix = "f"
		case TYPE_DOUBLE
			if( instr( *n->text, "." ) = 0 ) then
				suffix = "d"
			end if
		end select

		if( n->attrib and ASTATTRIB_OCT ) then
			s += "&o"
		elseif( n->attrib and ASTATTRIB_HEX ) then
			s += "&h"
		end if
		s += *n->text
		s += suffix

		if( need_rparen ) then
			s += ")"
		end if

	case ASTCLASS_ID
		if( n->attrib and ASTATTRIB_PARENTHESIZEDMACROPARAM ) then
			s += "("
		end if
		s += *n->text
		if( n->attrib and ASTATTRIB_PARENTHESIZEDMACROPARAM ) then
			s += ")"
		end if

	case ASTCLASS_TAGID
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
			case CH_DQUOTE    : s += $""""""

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
			s = "wstr(" + s + ")"
		end if

		if( n->class = ASTCLASS_CHAR ) then
			s = "asc(" + s + ")"
		end if

	case ASTCLASS_ORELSE      : s = hParens( emitL( n ) + " orelse "  + emitR( n ), need_parens )
	case ASTCLASS_ANDALSO     : s = hParens( emitL( n ) + " andalso " + emitR( n ), need_parens )
	case ASTCLASS_OR          : s = hParens( emitL( n ) + " or "      + emitR( n ), need_parens )
	case ASTCLASS_XOR         : s = hParens( emitL( n ) + " xor "     + emitR( n ), need_parens )
	case ASTCLASS_AND         : s = hParens( emitL( n ) + " and "     + emitR( n ), need_parens )
	case ASTCLASS_EQ          : s = hParens( emitL( n ) + " = "       + emitR( n ), need_parens )
	case ASTCLASS_NE          : s = hParens( emitL( n ) + " <> "      + emitR( n ), need_parens )
	case ASTCLASS_LT          : s = hParens( emitL( n ) + " < "       + emitR( n ), need_parens )
	case ASTCLASS_LE          : s = hParens( emitL( n ) + " <= "      + emitR( n ), need_parens )
	case ASTCLASS_GT          : s = hParens( emitL( n ) + " > "       + emitR( n ), need_parens )
	case ASTCLASS_GE          : s = hParens( emitL( n ) + " >= "      + emitR( n ), need_parens )
	case ASTCLASS_SHL         : s = hParens( emitL( n ) + " shl "     + emitR( n ), need_parens )
	case ASTCLASS_SHR         : s = hParens( emitL( n ) + " shr "     + emitR( n ), need_parens )
	case ASTCLASS_ADD         : s = hParens( emitL( n ) + " + "       + emitR( n ), need_parens )
	case ASTCLASS_SUB         : s = hParens( emitL( n ) + " - "       + emitR( n ), need_parens )
	case ASTCLASS_MUL         : s = hParens( emitL( n ) + " * "       + emitR( n ), need_parens )
	case ASTCLASS_DIV         : s = hParens( emitL( n ) + " / "       + emitR( n ), need_parens )
	case ASTCLASS_MOD         : s = hParens( emitL( n ) + " mod "     + emitR( n ), need_parens )
	case ASTCLASS_INDEX       : s =          emitL( n ) + "["         + emitR( n ) + "]"
	case ASTCLASS_MEMBER      : s =          emitL( n ) + "."         + emitR( n )
	case ASTCLASS_MEMBERDEREF : s =          emitL( n ) + "->"        + emitR( n )
	case ASTCLASS_STRCAT      : s = hParens( emitL( n ) + " + "       + emitR( n ), need_parens )

	case ASTCLASS_NOT       : s = hParens( "not "     + emitL( n ), need_parens )
	case ASTCLASS_NEGATE    : s = hParens( "-"        + emitL( n ), need_parens )
	case ASTCLASS_UNARYPLUS : s = hParens( "+"        + emitL( n ), need_parens )
	case ASTCLASS_DEFINED   : s =          "defined(" + emitAst( n->head ) + ")"
	case ASTCLASS_ADDROF    : s = hParens( "@"        + emitL( n ), need_parens )
	case ASTCLASS_DEREF     : s = hParens( "*"        + emitL( n ), need_parens )
	case ASTCLASS_STRINGIFY : s =          "#"        + emitAst( n->head )
	case ASTCLASS_SIZEOF    : s =          "sizeof("  + emitAst( n->head ) + ")"
	case ASTCLASS_CAST
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
			s += emitType( n ) + ", "
		end select
		s += emitAst( n->head ) + ")"

	case ASTCLASS_IIF
		s += "iif(" + _
			emitAst( n->expr ) + ", " + _
			emitAst( n->head ) + ", " + _
			emitAst( n->tail ) + ")"

	case ASTCLASS_PPMERGE
		var i = n->head
		while( i )
			if( i <> n->head ) then s += "##"
			s += emitAst( i )
			i = i->next
		wend

	case ASTCLASS_CALL
		s = *n->text + hParamList( n )

	case ASTCLASS_STRUCTINIT
		s = hParamList( n )

	case ASTCLASS_ARRAYINIT
		s = "{" + hSeparatedList( n, ", ", FALSE ) + "}"

	case ASTCLASS_DIMENSION
		if( n->expr ) then
			if( n->expr->class = ASTCLASS_CONSTI ) then
				s += "0 to " & (astEvalConstiAsInt64( n->expr ) - 1)
			else
				s += "0 to " + emitAst( n->expr, TRUE ) + " - 1"
			end if
		else
			s += "0 to ..."
		end if

	case ASTCLASS_TYPE
		s = emitType( n )

	case else
		astDump( n )
		assert( FALSE )
	end select

	function = s
end function

sub emitFile( byref filename as string, byval ast as ASTNODE ptr )
	emit.indent = 0
	emit.comment = 0
	emit.commentspaces = 0

	emit.fo = freefile( )
	if( open( filename, for output, as #emit.fo ) ) then
		oops( "could not open output file: '" + filename + "'" )
	end if

	var s = emitAst( ast )

	close #emit.fo
end sub
