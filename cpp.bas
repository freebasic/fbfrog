''
'' C pre-processor: CPP directive parsing, #if evaluation, macro expansion,
'' #include handling
''
'' cppMain() goes through the token buffer much like a C preprocessor would do,
'' parsing CPP directives keeping track of #defines and #undefs, doing macro
'' expansion, evaluating #if blocks, and expanding #includes.
''
'' All tokens that shouldn't be preserved for the C parser later are marked via
'' tkSetRemove() (for tkApplyRemoves() later). This affects most directives and
'' all tokens skipped in #if 0 blocks. As a special case, #defines and
'' unexpanded #includes are not deleted, but preserved for the C parser, because
'' we want to parse them there too.
''
'' Since directives are not deleted from the token buffer immediately, we can
'' leave #define bodies in place, and copy the tokens from there whenever they
'' are needed for a macro expansion. Otherwise it would be necessary to load the
'' tokens into some AST.
''
'' In various places (especially during macro expansion), we're temporarily
'' inserting helper tokens such as TK_ARGBEGIN/TK_ARGEND to enclose a range of
'' tokens. Then, instead of having to keep track of token indices which change
'' on every insertion/deletion, we just have to watch out for those specific
'' helper tokens to detect begin/end of the range.
''
'' Tokens inserted due to #include expansion are enclosed in TK_BEGININCLUDE and
'' TK_ENDINCLUDE. This allows detecting #include EOF for the #if/#include stack,
'' and allows the C parser to identify include boundaries later. If the include
'' file should be filtered out (according to -filterout/-filterin options), the
'' TK_BEGININCLUDE will be marked with TKFLAG_FILTEROUT, so we can mark all the
'' tokens between TK_BEGININCUDE/TK_ENDINCLUDE with TKFLAG_FILTEROUT, which lets
'' the C parser know which constructs should be marked with ASTATTRIB_FILTEROUT.
'' The include tokens can only be marked this way once the TK_ENDINCLUDE is
'' reached, because macro expansion may result in new tokens being inserted
'' which wouldn't automatically have the TKFLAG_FILTEROUT flag too.
''
'' #include statements themselves are not preserved as-is, but the filenames of
'' unexpanded direct #includes are recorded, so that frogReadAPI() can re-add
'' them to the top of the binding. "Unexpanded" here means #includes which were
'' either not found, or found but their content will be filtered out due to
'' -filterout. #includes which were found and are not going to be filtered out
'' will just be expanded in place. Generally, the point is that any #include
'' statements that won't be expanded in the final binding should be preserved
'' in the final binding, because they'll probably be needed.
''

#include once "fbfrog.bi"
#include once "crt.bi"
#include once "file.bi"

declare sub hMaybeExpandMacro( byref x as integer, byval inside_ifexpr as integer )

''
'' Check whether the number literal token (TK_NUMBER) is a valid number literal,
'' and build a CONSTI/CONSTF ASTNODE representing its value (the literal saved
'' as-is in textual form, without type suffixes) and data type.
''
'' As a special case, in CPP expressions, number literals are always treated as
'' 64bit signed or unsigned, no matter what dtype was given as suffix and
'' ignoring the "int" default.
''
'' These are covered:
''    123        (decimal)
''    .123       (decimal float)
''    123.123    (decimal float)
''    0xAABBCCDD (hexadecimal)
''    010        (octal)
''    010.0      (decimal float, not octal float)
''    0b10101    (binary; non-standard but supported by gcc/clang)
''    0          (we treat this as decimal, even though it's octal,
''               for prettier FB code: 0 vs &o0)
'' There also is some simple float exponent and type suffix parsing.
''
'' We have to parse the number literal (but without type suffix) first before we
'' can decide whether it's an integer or float. This decides whether a leading
'' zero indicates octal or not.
''
function hNumberLiteral _
	( _
		byval x as integer, _
		byval is_cpp as integer, _
		byref errmsg as string, _
		byval filterout as integer _
	) as ASTNODE ptr

	assert( tkGet( x ) = TK_NUMBER )
	dim as ubyte ptr p = tkGetText( x )

	var numbase = 10
	var is_float = FALSE
	var have_nonoct_digit = FALSE

	'' Check for non-decimal prefixes:
	'' 0, 0x, 0X, 0b, 0B
	if( p[0] = CH_0 ) then '' 0
		select case( p[1] )
		case CH_L_B, CH_B  '' 0b, 0B
			p += 2
			numbase = 2
		case CH_L_X, CH_X  '' 0x, 0X
			p += 2
			numbase = 16
		case CH_0 to CH_9
			p += 1
			numbase = 8
		end select
	end if

	'' Body (integer part + fractional part, if any)
	var begin = p

	select case( numbase )
	case 2
		while( (p[0] = CH_0) or (p[0] = CH_1) )
			p += 1
		wend

	case 16
		do
			select case as const( p[0] )
			case CH_0 to CH_9, CH_A to CH_F, CH_L_A to CH_L_F
			case else
				exit do
			end select
			p += 1
		loop

	case else
		do
			select case as const( p[0] )
			case CH_0 to CH_7
				'' These digits are allowed in both dec/oct literals
			case CH_8, CH_9
				'' These digits are only allowed in dec literals, not
				'' oct, but we don't know which it is yet.
				have_nonoct_digit = TRUE
			case CH_DOT
				'' Only one dot allowed
				if( is_float ) then exit do
				is_float = TRUE
			case else
				exit do
			end select
			p += 1
		loop
	end select

	'' Exponent? (can be used even without fractional part, e.g. '1e1')
	select case( p[0] )
	case CH_E, CH_L_E   '' 'E', 'e'
		is_float = TRUE
		p += 1

		'' ['+' | '-']
		select case( p[0] )
		case CH_PLUS, CH_MINUS
			p += 1
		end select

		'' ['0'-'9']*
		while( (p[0] >= CH_0) and (p[0] <= CH_9) )
			p += 1
		wend
	end select

	'' Floats with leading zeroes are decimal, not octal.
	if( is_float and (numbase = 8) ) then
		numbase = 10
	end if

	if( have_nonoct_digit and (numbase = 8) ) then
		errmsg = "invalid digit in octal number literal"
		exit function
	end if

	'' Save the number literal body (we don't want to include type suffixes here)
	var old = p[0]
	p[0] = 0  '' temporary null terminator
	var n = astNew( ASTCLASS_CONSTI, cptr( zstring ptr, begin ) )
	p[0] = old

	'' Float type suffixes
	select case( p[0] )
	case CH_F, CH_L_F    '' 'F' | 'f'
		p += 1
		is_float = TRUE
		n->dtype = TYPE_SINGLE
	case CH_D, CH_L_D    '' 'D' | 'd'
		p += 1
		is_float = TRUE
		n->dtype = TYPE_DOUBLE
	end select

	if( is_float ) then
		n->class = ASTCLASS_CONSTF
	else
		'' Integer type suffixes:
		''  l, ll, ul, ull, lu, llu
		'' MSVC-specific ones:
		''  [u]i{8|16|32|64}
		'' All letters can also be upper-case.
		var have_u = FALSE
		var have_l = FALSE
		var have_ll = FALSE

		select case( p[0] )
		case CH_U, CH_L_U       '' u
			p += 1
			have_u = TRUE
		end select

		select case( p[0] )
		case CH_L, CH_L_L       '' l, [u]l
			p += 1
			select case( p[0] )
			case CH_L, CH_L_L       '' l, [u]ll
				p += 1
				have_ll = TRUE
			case else
				have_l = TRUE
			end select

			if( have_u = FALSE ) then
				select case( p[0] )
				case CH_U, CH_L_U       '' u, llu
					p += 1
					have_u = TRUE
				end select
			end if

		case CH_I, CH_L_I       '' i, [u]i
			select case( p[1] )
			case CH_8 : p += 2 '' i8
			case CH_1 : if( p[2] = CH_6 ) then : p += 3 : end if  '' i16
			case CH_3 : if( p[2] = CH_2 ) then : p += 3 : end if  '' i32
			case CH_6 : if( p[2] = CH_4 ) then : p += 3 : have_ll = TRUE : end if  '' i64
			end select
		end select

		'' In CPP mode, all integer literals are 64bit, the 'l' suffix is ignored
		if( is_cpp ) then
			n->dtype = iif( have_u, TYPE_ULONGINT, TYPE_LONGINT )
		'' In C mode, integer literals default to 'int', and suffixes are respected
		elseif( have_ll ) then
			n->dtype = iif( have_u, TYPE_ULONGINT, TYPE_LONGINT )
		elseif( have_l ) then
			n->dtype = iif( have_u, TYPE_CULONG, TYPE_CLONG )
			if( filterout = FALSE ) then
				api->uses_clong = TRUE
			end if
		else
			n->dtype = iif( have_u, TYPE_ULONG, TYPE_LONG )
		end if

		select case( numbase )
		case 16 : n->attrib or= ASTATTRIB_HEX
		case  8 : n->attrib or= ASTATTRIB_OCT
		case  2 : n->attrib or= ASTATTRIB_BIN
		end select
	end if

	'' Show error if we didn't reach the end of the number literal
	if( p[0] <> 0 ) then
		errmsg = "invalid suffix on number literal: '" + *cptr( zstring ptr, p ) + "'"
		exit function
	end if

	function = n
end function

'' MacroParameter =
''      Identifier         (named parameter)
''    | Identifier '...'   (named + variadic)
''    | '...'              (variadic, using __VA_ARGS__)
'' ('...' can only appear on the last parameter)
private sub hMacroParam( byref x as integer, byval macro as ASTNODE ptr )
	'' Identifier?
	dim id as zstring ptr
	if( tkGet( x ) >= TK_ID ) then
		id = tkSpellId( x )
		x += 1
	end if

	'' Shouldn't have seen a '...' yet
	assert( (macro->attrib and ASTATTRIB_VARIADIC) = 0 )
	var maybevariadic = 0

	'' '...'?
	if( tkGet( x ) = TK_ELLIPSIS ) then
		x += 1
		maybevariadic = ASTATTRIB_VARIADIC
		if( id = NULL ) then
			''    #define m(a, ...)
			'' must become
			''    #define m(a, __VA_ARGS__...)
			'' in FB, because in FB, the '...' parameter of variadic macros must always have a name,
			'' and using __VA_ARGS__ for that is the easiest solution, because then we don't need to
			'' go replacing that in the macro body.
			id = @"__VA_ARGS__"
		end if
	elseif( id = NULL ) then
		tkOopsExpected( x, "macro parameter (identifier or '...')" )
	end if

	var param = astNew( ASTCLASS_MACROPARAM, id )
	param->attrib or= maybevariadic
	astAppend( macro, param )
	macro->attrib or= maybevariadic
	macro->paramcount += 1
end sub

'' <no whitespace> '(' MacroParameters ')'
private sub hMacroParamList( byref x as integer, byval macro as ASTNODE ptr )
	assert( macro->paramcount = -1 )

	'' '(' directly behind #define identifier, no spaces in between?
	if( (tkGet( x ) = TK_LPAREN) and _
	    ((tkGetFlags( x ) and TKFLAG_BEHINDSPACE) = 0) ) then
		x += 1

		macro->paramcount = 0

		'' Not just '()'?
		if( tkGet( x ) <> TK_RPAREN ) then
			'' MacroParam (',' MacroParam)*
			do
				hMacroParam( x, macro )

				'' ','?
				if( tkGet( x ) <> TK_COMMA ) then
					exit do
				end if
				x += 1
			loop
		end if

		'' ')'?
		tkExpect( x, TK_RPAREN, "to close the parameter list in this macro declaration" )
		x += 1
	end if
end sub

function hDefineHead( byref x as integer ) as ASTNODE ptr
	'' Identifier? (keywords should be allowed to, so anything >= TK_ID)
	select case( tkGet( x ) )
	case is < TK_ID
		tkExpect( x, TK_ID, "behind #define" )
	case KW_DEFINED
		tkOops( x, "'defined' cannot be used as macro name" )
	end select
	var macro = astNewPPDEFINE( tkSpellId( x ) )
	x += 1

	hMacroParamList( x, macro )

	function = macro
end function

''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''

type DEFINEINFO
	xdefine	as integer  '' 1st token of the #define directive (the '#')
	xbody	as integer  '' 1st token of the #define body
	xeol	as integer  '' eol/eof token behind the #define body

	'' PPDEFINE node with information about the macro's parameters etc.
	macro	as ASTNODE ptr
end type

private function definfoNew( ) as DEFINEINFO ptr
	function = callocate( sizeof( DEFINEINFO ) )
end function

#define definfoDelete( definfo ) deallocate( definfo )

private function definfoClone( byval a as DEFINEINFO ptr ) as DEFINEINFO ptr
	var b = definfoNew( )
	b->xdefine = a->xdefine
	b->xbody   = a->xbody
	b->xeol    = a->xeol
	b->macro   = astClone( a->macro )
	function = b
end function

private sub definfoSetRemove( byval definfo as DEFINEINFO ptr )
	tkSetRemove( definfo->xdefine, definfo->xeol )
end sub

const DEFINEBODY_FLAGMASK = not (TKFLAG_REMOVE or TKFLAG_DEFINE)

'' Copy a #define body into some other place
private sub definfoCopyBody( byval definfo as DEFINEINFO ptr, byval x as integer )
	assert( x > definfo->xeol )
	tkCopy( x, definfo->xbody, definfo->xeol - 1, DEFINEBODY_FLAGMASK )
end sub

'' Compare two #defines and determine whether they are equal
private function definfoDefinesAreEqual( byval a as DEFINEINFO ptr, byval b as DEFINEINFO ptr ) as integer
	'' Check name and parameters
	if( astIsEqual( a->macro, b->macro ) = FALSE ) then
		exit function
	end if

	'' Check body
	if( tkSpell( a->xbody, a->xeol ) <> tkSpell( b->xbody, b->xeol ) ) then
		exit function
	end if

	function = TRUE
end function

type SAVEDMACRO
	id		as zstring ptr		'' Macro's name

	'' A DEFINEINFO object if the macro was defined when being saved,
	'' or NULL if it was undefined.
	definfo		as DEFINEINFO ptr
end type

private sub savedmacroDtor( byval macro as SAVEDMACRO ptr )
	deallocate( macro->id )
	definfoDelete( macro->definfo )
end sub

enum
	'' If stack states:
	STATE_FILE = 0  '' file context (fresh toplevel/#include file, no #if yet)
	STATE_IF        '' #if context, fresh
	STATE_TRUE      '' #if context, saw #if/#elseif TRUE (and thus, further #elseif TRUE's must be skipped)
	STATE_ELSE      '' #if context, saw #else (and no further #elseif/#else can be allowed)
end enum

const MAXSTACK = 128

namespace cpp
	dim shared as integer x  '' Current token index

	type STACKNODE
		state		as integer  '' STATE_*
		knownfile	as integer  '' Index into cpp.files array, if it's an #include context, or -1
		xbegininclude	as integer  '' STATE_FILE: Position of the TK_BEGININCLUDE, so cppEndInclude() doesn't have to search it
	end type

	'' #if/file context stack
	'' * starts out with only the toplevel file context
	'' * #if blocks and #include contexts are put on this stack
	'' * an #endif found in an #include won't be able to close an #if from
	''   the parent file, since the #include stack node is in the way, and
	''   must be popped first.
	dim shared stack(0 to MAXSTACK-1) as STACKNODE
	dim shared as integer level  '' Current top of stack

	'' Stack level which caused #if 0 skipping
	'' * Allows us to continue parsing #ifs/#endifs even while skipping an
	''   #if 0 block, and this way, to determine which #endif ends skipping
	dim shared as integer skiplevel

	'' Whether we're currently in an include file (directly or indirectly)
	'' which will be filtered out.
	''    0  = no
	''    1+ = yes
	dim shared as integer filteroutlevel

	'' Lookup table of macros known to be either defined or undefined.
	''   defined <=> data = a DEFINEINFO object
	'' undefined <=> data = NULL
	'' Even though unregistered symbols are implicitly undefined,
	'' registering them is useful to show the "assuming undefined" warning
	'' (and to only show it once).
	dim shared macros		as THASH

	'' Array of macros saved by #pragma push_macro, for use by #pragma
	'' pop_macro later.
	''  * push_macro saves the macro's current state - whether it's defined
	''    or not, and if it's defined, it's body/value.
	''  * push_macro even saves duplicate copies of the macro state: two
	''    equal push_macro's directly following each other create two stack
	''    entries, not one.
	''  * pop_macro restores the macro to the last saved state, overwriting
	''    the current value, #defining the macro again if it was #undef'ed
	''    in the meantime, or even #undef'ing it.
	''  * pop_macro does nothing if the stack for that macro is empty.
	'' Macros are appended to the array in the order they are saved. Popping
	'' a macro requires searching the array backwards for an entry for the
	'' given macro name. A hash table could be used instead, but that does
	'' not seem to be worth it.
	dim shared savedmacros		as SAVEDMACRO ptr
	dim shared savedmacrocount	as integer

	dim shared incdirs		as ASTNODE ptr

	'' #include exclude/include filters: GROUP of FILTEROUT/FILTERIN nodes,
	'' in the order they should be applied.
	dim shared filters		as ASTNODE ptr

	type KNOWNFILE
		incfile as zstring ptr  '' Normalized file name
		filterout as integer
		checked_guard as integer
		'' Include guard symbol, if this file has a pure include guard,
		'' otherwise NULL
		guard as zstring ptr
	end type

	'' Information about known files
	dim shared files as KNOWNFILE ptr
	dim shared filecount as integer
	dim shared filetb as THASH  '' data = index into files array

	'' The unique file names from all toplevel #include statements,
	'' in the order in which they appeared.
	dim shared directincludes as ASTNODE ptr
	dim shared directincludetb as THASH
end namespace

sub cppInit( )
	cpp.x = 0

	'' Toplevel file context
	with( cpp.stack(0) )
		.state = STATE_FILE
		.knownfile = -1
		.xbegininclude = -1
	end with
	cpp.level = 0
	cpp.skiplevel = MAXSTACK  '' No skipping yet
	cpp.filteroutlevel = 0

	hashInit( @cpp.macros, 4, TRUE )
	cpp.savedmacros = NULL
	cpp.savedmacrocount = 0
	cpp.incdirs = astNewGROUP( )
	cpp.filters = astNewGROUP( )

	cpp.files = NULL
	cpp.filecount = 0
	hashInit( @cpp.filetb, 4, FALSE )

	cpp.directincludes = astNewGROUP( )
	hashInit( @cpp.directincludetb, 4, FALSE )
end sub

sub cppEnd( )
	'' If anything is left on the stack at EOF, it can only be #ifs
	'' (#includes should be popped due to TK_ENDINCLUDE's already)
	if( cpp.level > 0 ) then
		assert( cpp.stack(cpp.level).state >= STATE_IF )
		tkOops( cpp.x, "missing #endif" )
	end if

	assert( cpp.filteroutlevel = 0 )

	scope
		for i as integer = 0 to cpp.macros.room - 1
			definfoDelete( cpp.macros.items[i].data )
		next
		hashEnd( @cpp.macros )
	end scope
	scope
		for i as integer = 0 to cpp.savedmacrocount - 1
			savedmacroDtor( @cpp.savedmacros[i] )
		next
		deallocate( cpp.savedmacros )
	end scope

	hashEnd( @cpp.filetb )
	scope
		for i as integer = 0 to cpp.filecount - 1
			with( cpp.files[i] )
				deallocate( .incfile )
				deallocate( .guard )
			end with
		next
		deallocate( cpp.files )
	end scope

	hashEnd( @cpp.directincludetb )
end sub

#define cppSkipping( ) (cpp.skiplevel <> MAXSTACK)
#define cppWillBeFilteredOut( ) (cpp.filteroutlevel >= 1)

sub cppAddIncDir( byval incdir as ASTNODE ptr )
	astAppend( cpp.incdirs, incdir )
end sub

sub cppAddFilter( byval filter as ASTNODE ptr )
	astAppend( cpp.filters, filter )
end sub

sub cppAppendIncludeDirective( byref filename as string, byval tkflags as integer )
	var code = "#include """ + filename + """" + !"\n"
	var x = tkGetCount( )
	lexLoadC( x, sourcebufferFromZstring( code, code, NULL ) )
	tkAddFlags( x, tkGetCount( ) - 1, TKFLAG_REMOVE or tkflags )
end sub

private function cppKeepIncludeContent( byref includefilename as string ) as integer
	var keep = TRUE

	var i = cpp.filters->head
	while( i )

		'' Apply filter if it would make a difference. No need to check
		'' FILTEROUT if already excluding the file; no need to check
		'' FILTERIN if already including it.
		if( keep = (i->class = ASTCLASS_FILTEROUT) ) then
			if( strMatch( includefilename, *i->text ) ) then
				keep = not keep
			end if
		end if

		i = i->next
	wend

	function = keep
end function

private function cppLookupMacro( byval id as zstring ptr ) as DEFINEINFO ptr
	var item = hashLookup( @cpp.macros, id, hashHash( id ) )
	if( item->s ) then
		function = item->data
	else
		function = NULL
	end if
end function

private function cppIsKnownSymbol( byval id as zstring ptr ) as integer
	function = (hashLookup( @cpp.macros, id, hashHash( id ) )->s <> NULL)
end function

private function cppIsMacroCurrentlyDefined( byval id as zstring ptr ) as integer
	function = (cppLookupMacro( id ) <> NULL)
end function

'' Add/overwrite a known macro definition (or register it as known undefined)
private sub cppAddMacro( byval id as zstring ptr, byval definfo as DEFINEINFO ptr )
	var hash = hashHash( id )
	var item = hashLookup( @cpp.macros, id, hash )
	if( item->s ) then
		definfoDelete( item->data )
		item->data = definfo
	else
		hashAdd( @cpp.macros, item, hash, id, definfo )
	end if
end sub

private sub cppAddKnownUndefined( byval id as zstring ptr )
	cppAddMacro( id, NULL )
end sub

private sub hSetRemoveOnCurrentDefine( byval id as zstring ptr )
	var definfo = cppLookupMacro( id )
	if( definfo ) then
		definfoSetRemove( definfo )
	end if
end sub

private sub cppDefineMacro( byval id as zstring ptr, byval definfo as DEFINEINFO ptr )
	'' If overwriting an existing #define, don't preserve it
	hSetRemoveOnCurrentDefine( id )

	'' Register/overwrite as known defined symbol
	cppAddMacro( id, definfo )
end sub

private sub cppUndefMacro( byval id as zstring ptr )
	'' If #undeffing an existing #define, don't preserve it
	hSetRemoveOnCurrentDefine( id )

	'' Register/overwrite as known undefined symbol
	cppAddKnownUndefined( id )
end sub

'' Append a new entry to the array of saved macros
private sub cppAppendSavedMacro( byval id as zstring ptr, byval definfo as DEFINEINFO ptr )
	cpp.savedmacrocount += 1
	cpp.savedmacros = reallocate( cpp.savedmacros, _
			cpp.savedmacrocount * sizeof( SAVEDMACRO ) )
	with( cpp.savedmacros[cpp.savedmacrocount-1] )
		.id = strDuplicate( id )
		.definfo = definfo
	end with
end sub

private sub cppRemoveSavedMacro( byval i as integer )
	assert( (i >= 0) and (i < cpp.savedmacrocount) )

	var p = cpp.savedmacros + i
	savedmacroDtor( p )
	cpp.savedmacrocount -= 1

	'' Remove array element from the middle of the array: move all elements
	'' behind it to the front, by 1 slot, to close the gap.
	var tail = cpp.savedmacrocount - i
	if( tail > 0 ) then
		memmove( p, p + 1, tail * sizeof( SAVEDMACRO ) )
	end if
end sub

private function cppLookupSavedMacro( byval id as zstring ptr ) as integer
	for i as integer = cpp.savedmacrocount - 1 to 0 step -1
		if( *cpp.savedmacros[i].id = *id ) then
			return i
		end if
	next
	function = -1
end function

private sub cppSaveMacro( byval id as zstring ptr )
	'' Check the macro's current state.
	'' If it's defined, we need to duplicate the DEFINEINFO object;
	'' otherwise, if it's undefined, we use NULL.
	var definfo = cppLookupMacro( id )
	if( definfo ) then
		definfo = definfoClone( definfo )
	end if

	cppAppendSavedMacro( id, definfo )
end sub

private sub cppRestoreMacro( byval id as zstring ptr )
	'' Search for the last saved macro for this id
	var i = cppLookupSavedMacro( id )
	if( i < 0 ) then
		exit sub
	end if

	'' Restore the macro state
	var savedmacro = @cpp.savedmacros[i]
	if( savedmacro->definfo ) then
		'' It was defined when saved, (re)-#define the macro
		cppDefineMacro( id, savedmacro->definfo )
		savedmacro->definfo = NULL
	else
		'' It was undefined when saved, #undef the macro
		cppUndefMacro( id )
	end if

	'' Remove the entry from the saved macros stack
	cppRemoveSavedMacro( i )
end sub

private function cppLookupOrAppendKnownFile _
	( _
		byval incfile as zstring ptr, _
		byval is_rootfile as integer, _
		byref prettyfile as string _
	) as integer

	var hash = hashHash( incfile )
	var item = hashLookup( @cpp.filetb, incfile, hash )
	if( item->s ) then
		return cint( item->data )
	end if

	incfile = strDuplicate( incfile )

	var i = cpp.filecount
	cpp.filecount += 1
	cpp.files = reallocate( cpp.files, cpp.filecount * sizeof( *cpp.files ) )

	clear( cpp.files[i], 0, sizeof( *cpp.files ) )
	with( cpp.files[i] )
		.incfile = incfile

		'' Check -filterout/-filterin
		.filterout = (not is_rootfile) andalso (not cppKeepIncludeContent( prettyfile ))
	end with

	hashAdd( @cpp.filetb, item, hash, incfile, cptr( any ptr, i ) )
	function = i
end function

private sub cppKnownFileSetGuard( byval i as integer, byval guard as zstring ptr )
	assert( (i >= 0) and (i < cpp.filecount) )
	with( cpp.files[i] )
		assert( .checked_guard = FALSE )
		assert( .guard = NULL )
		.checked_guard = TRUE
		.guard = strDuplicate( guard )
	end with
end sub

private sub cppKnownFileDropGuard( byval i as integer )
	with( cpp.files[i] )
		if( .guard ) then
			deallocate( .guard )
			.guard = NULL
		end if
	end with
end sub

'' Remap .h name to a .bi name. If it's a known system header, we can even remap
'' it to the corresponding FB header (in some cases it's not as simple as
'' replacing .h by .bi).
private sub hTranslateHeaderFileName( byref filename as string )
	filename = pathStripExt( filename )

	'' Is it one of the CRT headers? Remap it to crt/*.bi
	if( hashContains( @fbcrtheaderhash, filename, hashHash( filename ) ) ) then
		filename = "crt/" + filename
	end if

	filename += ".bi"
end sub

private sub cppAddDirectInclude( byref inctext as string )
	var filename = inctext
	hTranslateHeaderFileName( filename )

	var hash = hashHash( filename )
	var item = hashLookup( @cpp.directincludetb, filename, hash )
	if( item->s ) then
		'' Already exists; ignore.
		'' We don't want duplicate #include statements.
		exit sub
	end if

	astAppend( cpp.directincludes, astNew( ASTCLASS_PPINCLUDE, filename ) )

	hashAdd( @cpp.directincludetb, item, hash, cpp.directincludes->tail->text, NULL )
end sub

function cppTakeDirectIncludes( ) as ASTNODE ptr
	function = cpp.directincludes
	cpp.directincludes = NULL
end function

private sub cppEol( )
	if( tkGet( cpp.x ) <> TK_EOL ) then
		tkOopsExpected( cpp.x, "end-of-line behind CPP directive" )
	end if
	cpp.x += 1
end sub

private sub hCheckForUnknownSymbol( byval id as zstring ptr )
	if( cppIsKnownSymbol( id ) = FALSE ) then
		'' Unknown symbol; we're going to assume that it's undefined

		'' Show a warning if it seems to be useful; i.e. if it's not a reserved symbol,
		'' but one intended to be defined by the user.
		if( frog.verbose ) then
			if( strIsReservedIdInC( id ) = FALSE ) then
				frogPrint( "treating as undefined: " + *id )
			end if
		end if

		'' Register as known undefined
		'' This also prevents the above warning from being shown
		'' multiple times for a single symbol.
		cppAddKnownUndefined( id )
	end if
end sub

'' C operator precedence, starting at 1, higher value = higher precedence
dim shared as integer cprecedence(ASTCLASS_CLOGOR to ASTCLASS_IIF) = _
{ _
	 2, _ '' ASTCLASS_CLOGOR
	 3, _ '' ASTCLASS_CLOGAND
	 4, _ '' ASTCLASS_OR
	 5, _ '' ASTCLASS_XOR
	 6, _ '' ASTCLASS_AND
	 7, _ '' ASTCLASS_CEQ
	 7, _ '' ASTCLASS_CNE
	 8, _ '' ASTCLASS_CLT
	 8, _ '' ASTCLASS_CLE
	 8, _ '' ASTCLASS_CGT
	 8, _ '' ASTCLASS_CGE
	 0, _ '' ASTCLASS_EQ (unused)
	 9, _ '' ASTCLASS_SHL
	 9, _ '' ASTCLASS_SHR
	10, _ '' ASTCLASS_ADD
	10, _ '' ASTCLASS_SUB
	11, _ '' ASTCLASS_MUL
	11, _ '' ASTCLASS_DIV
	11, _ '' ASTCLASS_MOD
	13, _ '' ASTCLASS_INDEX
	13, _ '' ASTCLASS_MEMBER
	13, _ '' ASTCLASS_MEMBERDEREF
	 0, _ '' ASTCLASS_STRCAT (unused)
	12, _ '' ASTCLASS_CLOGNOT
	12, _ '' ASTCLASS_NOT
	12, _ '' ASTCLASS_NEGATE
	12, _ '' ASTCLASS_UNARYPLUS
	 0, _ '' ASTCLASS_CDEFINED (unused)
	 0, _ '' ASTCLASS_DEFINED (unused)
	12, _ '' ASTCLASS_ADDROF
	12, _ '' ASTCLASS_DEREF
	 0, _ '' ASTCLASS_STRINGIFY (unused)
	12, _ '' ASTCLASS_SIZEOF
	 0, _ '' ASTCLASS_CAST (unused)
	 1  _ '' ASTCLASS_IIF
}

type CPPVALUE
	vali as longint
	dtype as integer
end type


''
'' CPP expression parser and evaluator
''
'' - The expression parsing is based on a "precedence climbing" algorithm
''
'' - Operations are evaluated as intmax_t or uintmax_t, i.e. 64bit signed or
''   unsigned, as in gcc/clang
''
'' - For most unary/binary operations we need to check the operand in order to
''   determine the result dtype (signed -> unsigned promotion rules can affect
''   the result). Others (e.g. the logical and relational ones) always return
''   a signed int regardless of the operands' dtypes.
''
'' - &&, || and ?: operands must only be evaluated when actually reached, such
''   that we can ignore division by zero if it occurs on an irrelevant code
''   path. (same goes for our "assuming undefined" warnings though - they should
''   only be shown if it affects the outcome of the expression)
''
'' - The ?: ternary conditional operator is a special case: one of its operands
''   mustn't be evaluated, but we still need to determine its dtype to determine
''   the result dtype of the ?: operation. Because of this we have to
''   differentiate between "evaluation" and "dtype determination" modes.
''
'' - Taking care to produce C's 1|0 boolean values, instead of FB's -1|0
''
'' a =  -a
'' a =  a + b
'' a =  a ? b : c
''
'' a = operand for UOPs, lhs operand for BOPs, result value to return
'' b = rhs operand for BOPs
'' c = 3rd operand for ?: conditional
''
private sub cppExpression _
	( _
		byref a as CPPVALUE, _
		byval dtype_only as integer, _
		byval level as integer = 0 _
	)

	'' Unary prefix operators
	select case( tkGet( cpp.x ) )
	case TK_EXCL  '' !
		cpp.x += 1

		'' operand
		cppExpression( a, dtype_only, cprecedence(ASTCLASS_CLOGNOT) )

		a.vali = -(a.vali = 0)
		a.dtype = TYPE_LONGINT  '' ! operator always produces a signed int

	case TK_TILDE  '' ~
		cpp.x += 1

		'' operand
		cppExpression( a, dtype_only, cprecedence(ASTCLASS_NOT) )

		a.vali = not a.vali

	case TK_MINUS  '' -
		cpp.x += 1

		'' operand
		cppExpression( a, dtype_only, cprecedence(ASTCLASS_NEGATE) )

		a.vali = -a.vali

	case TK_PLUS  '' +
		cpp.x += 1

		'' operand
		cppExpression( a, dtype_only, cprecedence(ASTCLASS_UNARYPLUS) )

	'' Atoms
	case TK_LPAREN  '' '(' Expression ')'
		'' '('
		cpp.x += 1

		'' Expression
		cppExpression( a, dtype_only )

		'' ')'
		tkExpect( cpp.x, TK_RPAREN, "for '(...)' parenthesized expression" )
		cpp.x += 1

	case TK_NUMBER  '' Number literal
		dim errmsg as string
		var n = hNumberLiteral( cpp.x, TRUE, errmsg, TRUE )
		if( n = NULL ) then
			tkOops( cpp.x, errmsg )
		end if
		if( n->class = ASTCLASS_CONSTF ) then
			tkOops( cpp.x, "float literal in CPP expression" )
		end if

		assert( (n->dtype = TYPE_LONGINT) or (n->dtype = TYPE_ULONGINT) )
		a.vali = astEvalConstiAsInt64( n )
		a.dtype = n->dtype

		cpp.x += 1

	'' Unexpanded identifier: treated as a literal 0
	case TK_ID
		if( dtype_only = FALSE ) then
			hCheckForUnknownSymbol( tkSpellId( cpp.x ) )
		end if
		a.vali = 0
		a.dtype = TYPE_LONGINT

		cpp.x += 1

	'' DEFINED ['('] Identifier [')']
	case KW_DEFINED
		cpp.x += 1

		'' '('
		var have_parens = FALSE
		if( tkGet( cpp.x ) = TK_LPAREN ) then
			have_parens = TRUE
			cpp.x += 1
		end if

		'' Identifier
		if( tkGet( cpp.x ) < TK_ID ) then
			tkExpect( cpp.x, TK_ID, "as operand of DEFINED" )
		end if
		if( dtype_only = FALSE ) then
			var id = tkSpellId( cpp.x )
			hCheckForUnknownSymbol( id )
			'' defined()  ->  1|0
			a.vali = -cppIsMacroCurrentlyDefined( id )
		end if
		a.dtype = TYPE_LONGINT
		cpp.x += 1

		if( have_parens ) then
			'' ')'
			tkExpect( cpp.x, TK_RPAREN, "for DEFINED(...)" )
			cpp.x += 1
		end if

	case else
		tkOopsExpected( cpp.x, "expression" )
	end select

	'' Infix operators
	do
		dim op as integer
		select case as const( tkGet( cpp.x ) )
		case TK_QUEST    : op = ASTCLASS_IIF     '' ? (a ? b : c)
		case TK_PIPEPIPE : op = ASTCLASS_CLOGOR  '' ||
		case TK_AMPAMP   : op = ASTCLASS_CLOGAND '' &&
		case TK_PIPE     : op = ASTCLASS_OR      '' |
		case TK_CIRC     : op = ASTCLASS_XOR     '' ^
		case TK_AMP      : op = ASTCLASS_AND     '' &
		case TK_EQEQ     : op = ASTCLASS_CEQ     '' ==
		case TK_EXCLEQ   : op = ASTCLASS_CNE     '' !=
		case TK_LT       : op = ASTCLASS_CLT     '' <
		case TK_LTEQ     : op = ASTCLASS_CLE     '' <=
		case TK_GT       : op = ASTCLASS_CGT     '' >
		case TK_GTEQ     : op = ASTCLASS_CGE     '' >=
		case TK_LTLT     : op = ASTCLASS_SHL     '' <<
		case TK_GTGT     : op = ASTCLASS_SHR     '' >>
		case TK_PLUS     : op = ASTCLASS_ADD     '' +
		case TK_MINUS    : op = ASTCLASS_SUB     '' -
		case TK_STAR     : op = ASTCLASS_MUL     '' *
		case TK_SLASH    : op = ASTCLASS_DIV     '' /
		case TK_PERCENT  : op = ASTCLASS_MOD     '' %
		case else        : exit do
		end select

		'' Higher/same level means process now (takes precedence),
		'' lower level means we're done and the parent call will
		'' continue. The first call will start with level 0.
		var oplevel = cprecedence(op)
		if( oplevel < level ) then
			exit do
		end if
		'' Left associative?
		if( op <> ASTCLASS_IIF ) then
			oplevel += 1
		end if

		'' operator
		cpp.x += 1

		dim b as CPPVALUE

		select case( op )
		case ASTCLASS_CLOGOR  '' ||
			'' Parse rhs (don't evaluate if lhs was true)
			cppExpression( b, dtype_only or (a.vali <> 0), oplevel )
			a.vali = iif( a.vali, 1, iif( b.vali, 1, 0 ) )
			a.dtype = TYPE_LONGINT  '' || always produces a signed int

		case ASTCLASS_CLOGAND  '' &&
			'' Parse rhs (don't evaluate if lhs was false)
			cppExpression( b, dtype_only or (a.vali = 0), oplevel )
			a.vali = iif( a.vali, iif( b.vali, 1, 0 ), 0 )
			a.dtype = TYPE_LONGINT  '' && always produces a signed int

		case ASTCLASS_IIF
			'' Parse 2nd operand (don't evaluate if condition = false)
			cppExpression( b, dtype_only or (a.vali = 0), oplevel )

			'' ':'?
			tkExpect( cpp.x, TK_COLON, "for a?b:c iif operator" )
			cpp.x += 1

			'' Parse 3rd operand (don't evaluate if condition = true)
			dim c as CPPVALUE
			cppExpression( c, dtype_only or (a.vali <> 0), oplevel )

			a.vali = iif( a.vali, b.vali, c.vali )
			a.dtype = max( b.dtype, c.dtype )

		case else
			'' Parse rhs
			cppExpression( b, dtype_only, oplevel )

			'' If one operand is unsigned, promote both operands to unsigned.
			'' This also takes care of the result type, except for relational BOPs,
			'' which are handled below.
			a.dtype = max( a.dtype, b.dtype )

			if( dtype_only = FALSE ) then
				select case( op )
				case ASTCLASS_DIV, ASTCLASS_MOD
					if( b.vali = 0 ) then
						tkOops( cpp.x, "division by zero" )
					end if
				end select

				if( a.dtype = TYPE_ULONGINT ) then
					select case as const( op )
					case ASTCLASS_OR  : a.vali =   cunsg( a.vali ) or  cunsg( b.vali )
					case ASTCLASS_XOR : a.vali =   cunsg( a.vali ) xor cunsg( b.vali )
					case ASTCLASS_AND : a.vali =   cunsg( a.vali ) and cunsg( b.vali )
					case ASTCLASS_CEQ : a.vali = -(cunsg( a.vali ) =   cunsg( b.vali ))
					case ASTCLASS_CNE : a.vali = -(cunsg( a.vali ) <>  cunsg( b.vali ))
					case ASTCLASS_CLT : a.vali = -(cunsg( a.vali ) <   cunsg( b.vali ))
					case ASTCLASS_CLE : a.vali = -(cunsg( a.vali ) <=  cunsg( b.vali ))
					case ASTCLASS_CGT : a.vali = -(cunsg( a.vali ) >   cunsg( b.vali ))
					case ASTCLASS_CGE : a.vali = -(cunsg( a.vali ) >=  cunsg( b.vali ))
					case ASTCLASS_SHL : a.vali =   cunsg( a.vali ) shl cunsg( b.vali )
					case ASTCLASS_SHR : a.vali =   cunsg( a.vali ) shr cunsg( b.vali )
					case ASTCLASS_ADD : a.vali =   cunsg( a.vali ) +   cunsg( b.vali )
					case ASTCLASS_SUB : a.vali =   cunsg( a.vali ) -   cunsg( b.vali )
					case ASTCLASS_MUL : a.vali =   cunsg( a.vali ) *   cunsg( b.vali )
					case ASTCLASS_DIV : a.vali =   cunsg( a.vali ) \   cunsg( b.vali )
					case ASTCLASS_MOD : a.vali =   cunsg( a.vali ) mod cunsg( b.vali )
					case else         : assert( FALSE )
					end select
				else
					select case as const( op )
					case ASTCLASS_OR  : a.vali =   a.vali or  b.vali
					case ASTCLASS_XOR : a.vali =   a.vali xor b.vali
					case ASTCLASS_AND : a.vali =   a.vali and b.vali
					case ASTCLASS_CEQ : a.vali = -(a.vali =   b.vali)
					case ASTCLASS_CNE : a.vali = -(a.vali <>  b.vali)
					case ASTCLASS_CLT : a.vali = -(a.vali <   b.vali)
					case ASTCLASS_CLE : a.vali = -(a.vali <=  b.vali)
					case ASTCLASS_CGT : a.vali = -(a.vali >   b.vali)
					case ASTCLASS_CGE : a.vali = -(a.vali >=  b.vali)
					case ASTCLASS_SHL : a.vali =   a.vali shl b.vali
					case ASTCLASS_SHR : a.vali =   a.vali shr b.vali
					case ASTCLASS_ADD : a.vali =   a.vali +   b.vali
					case ASTCLASS_SUB : a.vali =   a.vali -   b.vali
					case ASTCLASS_MUL : a.vali =   a.vali *   b.vali
					case ASTCLASS_DIV : a.vali =   a.vali \   b.vali
					case ASTCLASS_MOD : a.vali =   a.vali mod b.vali
					case else         : assert( FALSE )
					end select
				end if
			end if

			'' Relational BOPs always produce a signed int
			select case( op )
			case ASTCLASS_CEQ, ASTCLASS_CNE, _
			     ASTCLASS_CLT, ASTCLASS_CLE, _
			     ASTCLASS_CGT, ASTCLASS_CGE
				a.dtype = TYPE_LONGINT
			end select
		end select
	loop
end sub

private function hCheckForMacroCall( byval x as integer ) as DEFINEINFO ptr
	assert( tkGet( x ) >= TK_ID )
	var id = tkSpellId( x )

	'' Is this id a macro?
	var definfo = cppLookupMacro( id )
	if( definfo = NULL ) then
		return NULL
	end if

	'' Only expand if not marked otherwise
	if( hashContains( @frog.idopt(OPT_NOEXPAND), id, hashHash( id ) ) or _
	    (tkGetFlags( x ) and TKFLAG_NOEXPAND) or _
	    (definfo->macro->attrib and ASTATTRIB_POISONED) ) then
		return NULL
	end if

	function = definfo
end function

const MAXARGS = 128

private sub hParseMacroCallArgs _
	( _
		byref x as integer, _
		byval macro as ASTNODE ptr, _
		byval argbegin as integer ptr, _
		byval argend as integer ptr, _
		byref argcount as integer _
	)

	'' Note: The macro call argument list must be parsed without doing
	'' macro expansion. Each argument individually must be expanded later,
	'' but not before the list has been parsed & split up into individual
	'' arguments. I.e. the commas or closing ')' cannot come from macro
	'' expansions.

	var is_variadic = ((macro->attrib and ASTATTRIB_VARIADIC) <> 0)

	'' For each arg in the input...
	var reached_lastarg = FALSE
	do
		if( argcount >= MAXARGS ) then
			tkOops( x, "macro call arg buffer too small, MAXARGS=" & MAXARGS )
		end if

		argbegin[argcount] = x

		'' Is this the argument for the last parameter of a variadic macro?
		'' We're going to read all the remaining tokens into this last argument,
		'' even commas, thus there won't be any other arguments following after this one.
		assert( (not is_variadic) or (not reached_lastarg) )
		reached_lastarg = (argcount = (macro->paramcount - 1))

		'' For each token that's part of this arg...
		var level = 0
		do
			select case( tkGet( x ) )
			case TK_LPAREN
				level += 1

			case TK_RPAREN
				if( level <= 0 ) then
					exit do
				end if
				level -= 1

			case TK_COMMA
				'' A toplevel comma ends the current arg, unless it's a "..." vararg,
				'' which just "absorbs" everything until the closing ')'.
				if( level <= 0 ) then
					if( (not is_variadic) or (not reached_lastarg) ) then
						exit do
					end if
				end if

			case TK_EOF
				tkOopsExpected( x, "')' to close macro call argument list" )
			end select

			x += 1
		loop

		argend[argcount] = x - 1
		argcount += 1

		'' ','?
		if( tkGet( x ) <> TK_COMMA ) then
			exit do
		end if
		x += 1
	loop

	'' It's ok to omit the arg(s) for the variadic parameter of a variadic macro.
	if( is_variadic and (not reached_lastarg) ) then
		if( argcount >= MAXARGS ) then
			tkOops( x, "macro call arg buffer too small, MAXARGS=" & MAXARGS )
		end if
		argbegin[argcount] = x
		argend[argcount] = x - 1
		argcount += 1
	end if

	'' Not the expected amount of args?
	if( argcount <> macro->paramcount ) then
		dim s as string
		if( argcount > macro->paramcount ) then
			s = "too many"
		else
			s = "not enough"
		end if
		s += " arguments for '" + *macro->text + "' macro call: "
		s &= argcount & " given, " & macro->paramcount & " needed"
		tkOops( x, s )
	end if
end sub

private function hParseMacroCall _
	( _
		byval x as integer, _
		byval macro as ASTNODE ptr, _
		byval argbegin as integer ptr, _
		byval argend as integer ptr, _
		byref argcount as integer _
	) as integer

	var begin = x

	'' ID
	assert( tkGet( x ) >= TK_ID )
	x += 1

	argcount = -1

	'' Not just "#define m"?
	if( macro->paramcount >= 0 ) then
		'' '('?
		if( tkGet( x ) <> TK_LPAREN ) then
			return -1
		end if
		x += 1

		argcount = 0

		'' Not just "#define m()"?
		if( macro->paramcount > 0 ) then
			'' Parse the argument list and fill the argbegin() and
			'' argend() arrays accordingly
			hParseMacroCallArgs( x, macro, argbegin, argend, argcount )
		end if

		'' ')'?
		tkExpect( x, TK_RPAREN, "to close macro call argument list" )
		x += 1
	end if

	function = x - 1
end function

'' DEFINED ['('] Identifier [')']
private sub hSkipDefinedUop( byref x as integer )
	assert( tkGet( x ) = KW_DEFINED )
	x += 1

	'' '('?
	var have_lparen = FALSE
	if( tkGet( x ) = TK_LPAREN ) then
		have_lparen = TRUE
		x += 1
	end if

	'' Identifier? (not doing any expansion here)
	if( tkGet( x ) >= TK_ID ) then
		x += 1
	end if

	'' ')'?
	if( have_lparen ) then
		if( tkGet( x ) = TK_RPAREN ) then
			x += 1
		end if
	end if
end sub

private sub hWrapInTkBeginEnd( byval first as integer, byval last as integer )
	assert( first <= last )
	tkInsert( first, TK_BEGIN )
	last += 1
	tkInsert( last + 1, TK_END )
end sub

private sub hUnwrapTkBeginEnd( byval first as integer, byval last as integer )
	assert( tkGet( first ) = TK_BEGIN )
	assert( tkGet( last ) = TK_END )
	tkRemove( first, first )
	last -= 1
	tkRemove( last, last )
end sub

private function hExpandInTkBeginEnd _
	( _
		byval x as integer, _
		byval inside_ifexpr as integer _
	) as integer

	assert( tkGet( x ) = TK_BEGIN )

	do
		select case( tkGet( x ) )
		case TK_END
			exit do

		case KW_DEFINED
			'' If inside an #if condition expression, don't expand symbols behind the defined operator.
			'' According to the C standard, the handling of defined's that result from macro expansion
			'' is undefined, but gcc handles them as normal defined's, so we do too.
			if( inside_ifexpr ) then
				hSkipDefinedUop( x )
				x -= 1
			end if

		case is >= TK_ID
			hMaybeExpandMacro( x, inside_ifexpr )
		end select

		x += 1
	loop

	function = x
end function

private function hExpandInRange _
	( _
		byval first as integer, _
		byval last as integer, _
		byval inside_ifexpr as integer _
	) as integer

	'' Do nothing if range is empty - happens when expanding in a macro
	'' expansion but the expansion is empty, or when expanding in an #if
	'' condition but it's missing.
	if( first > last ) then
		return last
	end if

	'' Insert TK_BEGIN/TK_END around the argument's tokens, to prevent the
	'' macro call parsing functions from reading out-of-bounds.
	hWrapInTkBeginEnd( first, last )
	last += 2
	assert( tkGet( last ) = TK_END )

	'' Expand anything in the range
	last = hExpandInTkBeginEnd( first, inside_ifexpr )

	'' Remove TK_BEGIN/TK_END again
	hUnwrapTkBeginEnd( first, last )
	last -= 2

	function = last
end function

'' Set or unset the BEHINDSPACE flag of a token
private sub hOverrideBehindspace( byval x as integer, byval flag as integer )
	tkSetFlags( x, (tkGetFlags( x ) and (not TKFLAG_BEHINDSPACE)) or flag )
end sub

''
'' - Macro arguments must be inserted in place of macro parameters, and fully
''   macro-expanded, but only self-contained without help from tokens outside
''   the argument.
''
'' - Arguments used with # mustn't be macro-expanded, and for arguments used
''   with ##, the last/first token musn't be macro-expanded depending on whether
''   the parameter was on the lhs/rhs of the ## (but the rest of the argument's
''   tokens that aren't used by the ##, if any, must be macro-expanded).
''   I.e. macro expansion mustn't be done when parsing the arguments, but later
''   when inserting them in place of parameters, with the given restrictions.
''
'' - # or ## tokens coming from arguments must not be treated as stringify/merge
''   operators. This must be done only for # or ## in the macro body.
''
'' - #stringify operations must be solved before ## merging (e.g. <L ## #param>
''   becomes <L"argtext">)
''
'' - ## operands may be empty: if an argument is used with ##, but the argument
''   is empty, then the ## doesn't merge anything. ## with 2 empty operands
''   is removed completely. Macro body token(s) preceding/following the ##
''   operand are not taken into account for the merge. Empty ## operand doesn't
''   cause preceding/following tokens to be used instead.
''
'' - If a macro parameter expands to multiple tokens, ## affects the last/first
''   token from the lhs/rhs operands respectively, but not all the tokens
''   inserted in place of the parameter(s).
''
private function hInsertMacroExpansion _
	( _
		byval callbehindspace as integer, _
		byval expansionbegin as integer, _
		byval definfo as DEFINEINFO ptr, _
		byval argbegin as integer ptr, _
		byval argend as integer ptr, _
		byval argcount as integer, _
		byval inside_ifexpr as integer _
	) as integer

	'' Insert the macro body tokens from AST into the tk buffer, surrounded
	'' with TK_BEGIN/TK_END, to allow the code below to read "out-of-bounds"
	'' by -1 or +1, which simplifies handling of # and ## operators.
	''
	'' Having the TK_END also removes the need to keep track of the end of
	'' the expansion through all the insertions/deletions done here.
	'' Instead, if we need to know the end of the expansion, we can just
	'' look for the TK_END.
	tkInsert( expansionbegin, TK_END )
	definfoCopyBody( definfo, expansionbegin )
	tkInsert( expansionbegin, TK_BEGIN )

	'' Update the BEHINDSPACE status of the first token in the expansion to
	'' be the same as that of the macro name which we're expanding
	hOverrideBehindspace( expansionbegin + 1, callbehindspace )

	'' Solve #stringify operators (higher priority than ##, and no macro
	'' expansion done for the arg)
	var x = expansionbegin + 1
	while( tkGet( x ) <> TK_END )

		'' '#param'?
		if( tkGet( x ) = TK_HASH ) then
			'' Followed by identifier?
			if( tkGet( x + 1 ) >= TK_ID ) then
				'' Is it a macro parameter?
				var arg = astLookupMacroParam( definfo->macro, tkSpellId( x + 1 ) )
				if( arg >= 0 ) then
					'' Remove #param, and insert stringify result instead
					'' but preserve BEHINDSPACE status.
					assert( (arg >= 0) and (arg < argcount) )
					var behindspace = tkGetFlags( x ) and TKFLAG_BEHINDSPACE
					tkRemove( x, x + 1 )
					tkInsert( x, TK_STRING, tkSpell( argbegin[arg], argend[arg] ) )
					hOverrideBehindspace( x, behindspace )
				end if
			end if
		end if

		x += 1
	wend

	'' Replace ## tokens by special internal merge operator tokens, so that
	'' ## tokens from macro arguments aren't mistaken for merge operators.
	x = expansionbegin + 1
	while( tkGet( x ) <> TK_END )

		'' '##'?
		if( tkGet( x ) = TK_HASHHASH ) then
			tkInsert( x, TK_PPMERGE )
			var y = x + 1
			tkSetLocation( x, tkGetLocation( y ) )
			tkRemove( y, y )
		end if

		x += 1
	wend

	'' Insert args into params, surrounded with TK_ARGBEGIN/END, so that
	'' - we know when an arg was empty when doing ## merging (to avoid
	''   merging with other tokens outside the arg),
	'' - we know the arg's boundaries for macro-expanding it later. (must be
	''   done after merging, because only the unmerged tokens of an arg
	''   shall be macro-expanded, and not the ones involved in merging)
	x = expansionbegin + 1
	while( tkGet( x ) <> TK_END )

		'' Macro parameter?
		if( tkGet( x ) >= TK_ID ) then
			var arg = astLookupMacroParam( definfo->macro, tkSpellId( x ) )
			if( arg >= 0 ) then
				'' >= TK_ID
				var behindspace = tkGetFlags( x ) and TKFLAG_BEHINDSPACE
				tkRemove( x, x )

				'' TK_ARGBEGIN
				tkInsert( x, TK_ARGBEGIN )
				x += 1

				'' arg's tokens
				tkCopy( x, argbegin[arg], argend[arg], DEFINEBODY_FLAGMASK )
				hOverrideBehindspace( x, behindspace )
				x += argend[arg] - argbegin[arg] + 1

				'' TK_ARGEND
				tkInsert( x, TK_ARGEND )
			end if
		end if

		x += 1
	wend

	''
	'' Do '##' merging
	''
	'' It's not clear how <a ## ## b> or <a ## b ## c> should be processed
	'' (undefined behaviour), so fbfrog shows an error about the first
	'' (cannot merge a and ##) and processes the 2nd as (a##b)##c, i.e.
	'' left-associative.
	''
	x = expansionbegin + 1
	while( tkGet( x ) <> TK_END )

		'' '##' from original macro body (and not '##' from a macro argument)?
		if( tkGet( x ) = TK_PPMERGE ) then

			'' 1. If lhs/rhs of '##' were params, then now there will be TK_ARGBEGIN,...,TK_ARGEND sequences.
			'' Move last/first token out of the arg boundaries, so that they end up right next to the '##'.
			'' (can just move the TK_ARGEND/TK_ARGBEGIN respectively, that's easier & faster)
			''
			'' Example with arg on both sides:
			'' from:
			''    [argbegin] a b [argend] ## [argbegin] c d [argend]
			'' to:
			''    [argbegin] a [argend] b ## c [argbegin] d [argend]
			''
			'' If this causes an TK_ARGBEGIN/END to become empty, it must be removed,
			'' so that it won't be misinterpreted as empty arg operand for a following ## operator:
			'' from:
			''    [argbegin] a [argend] ## [argbegin] b [argend] ## [argbegin] c [argend]
			'' to:
			''    a##b ## [argbegin] c [argend]
			'' in order to avoid the situation where the 2nd ##'s lhs seems to be an empty arg:
			''    [argbegin] [argend] a ## b [argbegin] [argend] ## [argbegin] c [argend]
			'' because actually the merged "ab" token is supposed to be 2nd ##'s lhs.

			'' lhs was a non-empty arg?
			if( (tkGet( x - 1 ) = TK_ARGEND) and (tkGet( x - 2 ) <> TK_ARGBEGIN)  ) then
				tkRemove( x - 1, x - 1 )
				tkInsert( x - 2, TK_ARGEND )
				assert( tkGet( x ) = TK_PPMERGE )
				assert( tkGet( x - 1 ) <> TK_ARGEND )
				assert( tkGet( x - 2 ) = TK_ARGEND )

				'' Empty now? Then remove the TK_ARGBEGIN/END
				if( tkGet( x - 3 ) = TK_ARGBEGIN ) then
					tkRemove( x - 3, x - 2 )
					x -= 2
				end if
			end if

			'' rhs was a non-empty arg?
			if( (tkGet( x + 1 ) = TK_ARGBEGIN) and (tkGet( x + 2 ) <> TK_ARGEND) ) then
				tkRemove( x + 1, x + 1 )
				tkInsert( x + 2, TK_ARGBEGIN )
				assert( tkGet( x ) = TK_PPMERGE )
				assert( tkGet( x + 1 ) <> TK_ARGBEGIN )
				assert( tkGet( x + 2 ) = TK_ARGBEGIN )

				'' Empty now? Then remove the TK_ARGBEGIN/END
				if( tkGet( x + 3 ) = TK_ARGEND ) then
					tkRemove( x + 2, x + 3 )
				end if
			end if

			assert( tkGet( x ) = TK_PPMERGE )
			var l = x - 1
			var r = x + 1

			'' If one operand was an empty arg, then no merging needs to be done,
			'' the other operand can just be preserved as-is; or in case both were
			'' empty, the ## just disappears.

			'' Non-empty on both sides?
			if( (tkGet( l ) <> TK_ARGEND) and (tkGet( r ) <> TK_ARGBEGIN) ) then
				if( tkGet( l ) = TK_BEGIN ) then
					tkOops( x, "## merge operator at beginning of macro body, missing operand to merge with" )
				end if
				if( tkGet( r ) = TK_END ) then
					tkOops( x, "## merge operator at end of macro body, missing operand to merge with" )
				end if

				'' Combine the original text representation of both tokens,
				'' and prepend a space if the lhs was BEHINDSPACE, such that
				'' the merged token will also be BEHINDSPACE.
				dim mergetext as string
				if( tkGetFlags( l ) and TKFLAG_BEHINDSPACE ) then
					mergetext += " "
				end if
				mergetext += tkSpell( l ) + tkSpell( r )

				'' and try to lex them
				var y = tkGetCount( )
				lexLoadC( y, sourcebufferFromZstring( "## merge operation", mergetext, tkGetLocation( x ) ) )

				'' That should have produced only 1 token. If it produced more, then the merge failed.
				assert( tkGetCount( ) >= (y + 1) )
				if( tkGetCount( ) > (y + 1) ) then
					tkRemove( y, tkGetCount( ) - 1 )
					tkOops( x, "## merge operator cannot merge '" + tkSpell( x - 1 ) + "' and '" + tkSpell( x + 1 ) + "'" )
				end if

				'' Remove the 3 (l ## r) tokens and insert the merged token in place of l
				tkRemove( l, r )
				y -= 3
				x = l

				tkCopy( x, y, y, DEFINEBODY_FLAGMASK )
				y += 1

				tkRemove( y, y )
			else
				'' Just remove the '##'
				tkRemove( x, x )
				x -= 1
			end if
		end if

		x += 1
	wend

	'' Recursively macro-expand the tokens in each TK_ARGBEGIN/END sequence,
	'' and then remove TK_ARGBEGIN/END.
	x = expansionbegin + 1
	while( tkGet( x ) <> TK_END )

		'' Macro parameter?
		if( tkGet( x ) = TK_ARGBEGIN ) then
			var y = x
			do
				y += 1
			loop while( tkGet( y ) <> TK_ARGEND )

			'' Macro-expand the arg's tokens
			y = hExpandInRange( x, y, inside_ifexpr )

			'' Remove TK_ARGBEGIN/END wrapping
			assert( tkGet( x ) = TK_ARGBEGIN )
			tkRemove( x, x )
			x -= 1
			y -= 1
			assert( tkGet( y ) = TK_ARGEND )
			tkRemove( y, y )
			y -= 1

			x = y
		end if

		x += 1
	wend

	'' Remove the TK_BEGIN/END wrapping around the expansion
	assert( tkGet( expansionbegin ) = TK_BEGIN )
	tkRemove( expansionbegin, expansionbegin )
	x -= 1
	assert( tkGet( x ) = TK_END )
	tkRemove( x, x )
	x -= 1

	function = x
end function

private sub hExpandMacro _
	( _
		byval definfo as DEFINEINFO ptr, _
		byval callbegin as integer, _
		byval callend as integer, _
		byval argbegin as integer ptr, _
		byval argend as integer ptr, _
		byval argcount as integer, _
		byval inside_ifexpr as integer _
	)

	'' Insert the macro body behind the call (this way the positions
	'' stored in argbegin()/argend() stay valid)
	var expansionbegin = callend + 1
	var expansionend = hInsertMacroExpansion( _
			tkGetFlags( callbegin ) and TKFLAG_BEHINDSPACE, _
			expansionbegin, definfo, argbegin, argend, argcount, inside_ifexpr )

	'' Set expansion level on the expansion tokens:
	'' = minlevel from macro call tokens + 1
	'' before doing nested macro expansion in the expansion tokens.
	tkSetExpansionLevel( expansionbegin, expansionend, _
		tkGetExpansionLevel( _
			tkFindTokenWithMinExpansionLevel( callbegin, callend ) _
		) + 1 )

	'' Recursively do macro expansion in the expansion
	'' - Marking the current macro as poisoned, so it won't be expanded
	''   again within the expansion, preventing expansion of complete
	''   recursive calls.
	'' - Incomplete recursive calls need to be marked with NOEXPAND so they
	''   won't be expanded later when they become complete by taking into
	''   account tokens following behind the expansion.
	definfo->macro->attrib or= ASTATTRIB_POISONED
	expansionend = hExpandInRange( expansionbegin, expansionend, inside_ifexpr )
	definfo->macro->attrib and= not ASTATTRIB_POISONED

	'' Disable future expansion of recursive macro calls to this macro
	'' (those that weren't expanded due to the "poisoning")
	scope
		var x = expansionbegin
		while( x <= expansionend )

			if( tkGet( x ) >= TK_ID ) then
				'' Known macro, and it's the same as this one?
				var calldefinfo = hCheckForMacroCall( x )
				if( calldefinfo = definfo ) then
					'' Can the macro call be parsed successfully,
					'' and is it fully within the expansion?
					dim as integer argbegin(0 to MAXARGS-1)
					dim as integer argend(0 to MAXARGS-1)
					dim as integer argcount
					var callend = hParseMacroCall( x, definfo->macro, @argbegin(0), @argend(0), argcount )
					if( (callend >= 0) and (callend <= expansionend) ) then
						tkAddFlags( x, x, TKFLAG_NOEXPAND )
					end if
				end if
			end if

			x += 1
		wend
	end scope

	'' Then remove the call tokens
	tkRemove( callbegin, callend )
end sub

private sub hMaybeExpandMacro( byref x as integer, byval inside_ifexpr as integer )
	var begin = x

	var definfo = hCheckForMacroCall( x )
	if( definfo = NULL ) then
		exit sub
	end if

	dim as integer argbegin(0 to MAXARGS-1)
	dim as integer argend(0 to MAXARGS-1)
	dim as integer argcount

	'' Try to parse the macro call (can fail in case of function-like macro
	'' without argument list)
	var callbegin = x
	var callend = hParseMacroCall( callbegin, definfo->macro, @argbegin(0), @argend(0), argcount )
	if( callend < 0 ) then
		exit sub
	end if

	hExpandMacro( definfo, callbegin, callend, @argbegin(0), @argend(0), argcount, inside_ifexpr )

	'' The macro call was replaced with the body, the token at the TK_ID's
	'' position must be re-parsed.
	x -= 1
end sub

private sub cppPush( byval state as integer, byval knownfile as integer = -1 )
	assert( iif( knownfile >= 0, state = STATE_FILE, TRUE ) )

	cpp.level += 1
	if( cpp.level >= MAXSTACK ) then
		tkOops( cpp.x, "#if/#include stack too small, MAXSTACK=" & MAXSTACK )
	end if

	with( cpp.stack(cpp.level) )
		.state = state
		.knownfile = knownfile
		.xbegininclude = -1
	end with

	if( knownfile >= 0 ) then
		if( cpp.files[knownfile].filterout ) then
			cpp.filteroutlevel += 1
		end if
	end if
end sub

private sub cppPop( )
	var knownfile = cpp.stack(cpp.level).knownfile
	if( knownfile >= 0 ) then
		if( cpp.files[knownfile].filterout ) then
			cpp.filteroutlevel -= 1
		end if
	end if
	cpp.level -= 1
end sub

private sub cppApplyIf( byval condition as integer )
	if( condition ) then
		'' #if TRUE, don't skip
		cpp.stack(cpp.level).state = STATE_TRUE
		cpp.skiplevel = MAXSTACK  '' needed for #elif, in case we were skipping previously
	else
		'' #if FALSE, start skipping (or in case of #elif, possibly continue)
		cpp.skiplevel = cpp.level
	end if
end sub

private function hSkipEols( byval x as integer ) as integer
	while( tkGet( x ) = TK_EOL )
		x += 1
	wend
	function = x
end function

private function cppIfExpr( ) as integer
	'' Expand macros in the #if condition before parsing it
	'' * but don't expand operands of the "defined" operator
	'' * we allow "defined" operators to be produced by
	''   macro expansion, like gcc
	hExpandInRange( cpp.x, hSkipToEol( cpp.x ) - 1, TRUE )

	'' Try to parse and evaluate an expression
	dim value as CPPVALUE
	cppExpression( value, FALSE )
	function = (value.vali <> 0)
end function

private sub cppIf( )
	cppPush( STATE_IF )
	cpp.x += 1

	if( cppSkipping( ) ) then
		exit sub
	end if

	'' Condition expression
	cppApplyIf( cppIfExpr( ) )

	cppEol( )
end sub

private sub cppIfdef( byval directivekw as integer )
	cppPush( STATE_IF )
	cpp.x += 1

	if( cppSkipping( ) ) then
		exit sub
	end if

	'' Identifier
	if( tkGet( cpp.x ) < TK_ID ) then
		tkExpect( cpp.x, TK_ID, "behind " + tkInfoPretty( directivekw ) )
	end if
	var id = tkSpellId( cpp.x )
	hCheckForUnknownSymbol( id )
	cpp.x += 1

	var condition = cppIsMacroCurrentlyDefined( id )
	if( directivekw = KW_IFNDEF ) then
		condition = not condition
	end if
	cppApplyIf( condition )

	cppEol( )
end sub

'' Forget the guard (if any) for the current file context
private sub cppDisableIncludeGuardOptimization( )
	assert( cpp.level >= 1 )
	assert( cpp.stack(cpp.level-1).state = STATE_FILE )
	var knownfile = cpp.stack(cpp.level-1).knownfile
	if( knownfile >= 0 ) then
		cppKnownFileDropGuard( knownfile )
	end if
end sub

'' Check whether we're inside the first nesting level inside a file
'' (for example, an #include guard)
private function cppInsideFileLevelBlock( ) as integer
	assert( cpp.stack(cpp.level).state <> STATE_FILE )
	if( cpp.level >= 1 ) then
		function = (cpp.stack(cpp.level-1).state = STATE_FILE)
	end if
end function

private sub cppElseIf( )
	'' Verify #elif usage even if skipping
	select case( cpp.stack(cpp.level).state )
	case is < STATE_IF
		tkOops( cpp.x, "#elif without #if" )
	case STATE_ELSE
		tkOops( cpp.x, "#elif after #else" )
	end select
	cpp.x += 1

	if( cppInsideFileLevelBlock( ) ) then
		cppDisableIncludeGuardOptimization( )
	end if

	'' Evaluate condition in case it matters:
	''    a) not yet skipping,
	''    b) skipping due to a previous #if/#elif FALSE
	if( (cpp.skiplevel = MAXSTACK) or (cpp.skiplevel = cpp.level) ) then
		'' But not if there already was an #if/#elif TRUE on this level
		'' (then this #elif isn't reached)
		if( cpp.stack(cpp.level).state = STATE_TRUE ) then
			'' Start/continue skipping
			cpp.skiplevel = cpp.level
		else
			'' Condition expression
			cppApplyIf( cppIfExpr( ) )
			cppEol( )
		end if
	end if
end sub

private sub cppElse( )
	'' Verify #else usage even if skipping
	select case( cpp.stack(cpp.level).state )
	case is < STATE_IF
		tkOops( cpp.x, "#else without #if" )
	case STATE_ELSE
		tkOops( cpp.x, "#else after #else" )
	end select
	cpp.x += 1

	if( cppInsideFileLevelBlock( ) ) then
		cppDisableIncludeGuardOptimization( )
	end if

	cppEol( )

	'' Check whether to skip this #else or not, if
	''    a) not yet skipping,
	''    b) skipping due to a previous #if/#elif FALSE
	if( (cpp.skiplevel = MAXSTACK) or (cpp.skiplevel = cpp.level) ) then
		if( cpp.stack(cpp.level).state = STATE_TRUE ) then
			'' Previous #if/#elseif TRUE, skip #else
			cpp.skiplevel = cpp.level
		else
			'' Previous #if/#elseif FALSE, don't skip #else
			cpp.skiplevel = MAXSTACK
		end if
	end if

	cpp.stack(cpp.level).state = STATE_ELSE
end sub

private sub cppEndIf( )
	if( cpp.stack(cpp.level).state < STATE_IF ) then
		tkOops( cpp.x, "#endif without #if" )
	end if
	cpp.x += 1

	cppEol( )

	if( cppInsideFileLevelBlock( ) ) then
		'' If we don't reach the #include EOF directly after the #endif,
		'' then this can't be an #include guard
		if( tkGet( hSkipEols( cpp.x ) ) <> TK_ENDINCLUDE ) then
			assert( tkGet( hSkipEols( cpp.x ) ) <> TK_EOF )
			cppDisableIncludeGuardOptimization( )
		end if
	end if

	'' If skipping due to current level, then stop skipping.
	if( cpp.skiplevel = cpp.level ) then
		cpp.skiplevel = MAXSTACK
	end if

	cppPop( )
end sub

'' Search for #included files in one of the parent directories of the context
'' file. Usually the #include will refer to a file in the same directory or in
'' a sub-directory at the same level or some levels up.
private function hSearchHeaderFile _
	( _
		byref contextfile as string, _
		byref inctext as string _
	) as string

	if( frog.verbose ) then
		frogPrint( "searching: " + inctext + " (context = " + contextfile + ")" )
	end if

	'' 1. If #including by absolute path, use it as-is
	if( pathIsAbsolute( inctext ) ) then
		return inctext
	end if

	'' 2. Relative to context file
	var incfile = pathAddDiv( pathOnly( contextfile ) ) + inctext
	if( frog.verbose ) then
		frogPrint( "trying: " + incfile )
	end if
	if( fileexists( incfile ) ) then
		return incfile
	end if

	'' 3. In any of the include search directories
	var i = cpp.incdirs->head
	while( i )

		incfile = pathAddDiv( *i->text ) + inctext
		if( frog.verbose ) then
			frogPrint( "trying: " + incfile )
		end if
		if( fileexists( incfile ) ) then
			return incfile
		end if

		i = i->next
	wend

	function = ""
end function

'' Check for the typical #include guard header:
''    #ifndef ID <EOL> #define ID ...
private function hDetectIncludeGuardBegin( byval first as integer ) as zstring ptr
	assert( tkGet( first - 2 ) = TK_BEGININCLUDE )
	assert( tkGet( first - 1 ) = TK_EOL )

	var x = hSkipEols( first )

	if( tkGet( x ) <> TK_HASH ) then exit function
	x += 1
	if( tkGet( x ) <> KW_IFNDEF ) then exit function
	x += 1
	if( tkGet( x ) <> TK_ID ) then exit function
	var id1 = tkGetText( x )
	x += 1
	if( tkGet( x ) <> TK_EOL ) then exit function
	x += 1
	if( tkGet( x ) <> TK_HASH ) then exit function
	x += 1
	if( tkGet( x ) <> KW_DEFINE ) then exit function
	x += 1
	if( tkGet( x ) <> TK_ID ) then exit function
	var id2 = tkGetText( x )
	if( *id1 <> *id2 ) then exit function

	function = id1
end function

private sub cppInclude( byval begin as integer )
	cpp.x += 1

	assert( cppSkipping( ) = FALSE )

	'' Expand macros behind the #include (but still in the same line)
	hExpandInRange( cpp.x, hSkipToEol( cpp.x ) - 1, FALSE )

	'' "filename"
	tkExpect( cpp.x, TK_STRING, "containing the #include file name" )

	'' Save the location for later, across the insertions/deletions below
	var location = *tkGetLocation( cpp.x )

	'' Internal #include for a root file? Don't apply -filterin/-filterout,
	'' don't do #include file search...
	var includetkflags = tkGetFlags( cpp.x )
	var is_rootfile = ((includetkflags and TKFLAG_ROOTFILE) <> 0)
	var is_preinclude = ((includetkflags and TKFLAG_PREINCLUDE) <> 0)

	dim as string contextfile
	if( location.source ) then
		contextfile = *location.source->name
	end if
	var inctext = *tkGetText( cpp.x )

	cpp.x += 1

	cppEol( )

	'' Mark #include directive for removal
	tkSetRemove( begin, cpp.x - 1 )

	dim incfile as string
	if( is_rootfile ) then
		incfile = inctext
	else
		incfile = hSearchHeaderFile( contextfile, inctext )
		if( len( incfile ) = 0 ) then
			frogPrint( inctext + " (not found)" )

			'' This #include statement couldn't be expanded, so we
			'' have to preserve it into the final binding, if it
			'' won't be filtered out itself.
			'' Pre-#includes should never be preserved though.
			if( (not cppWillBeFilteredOut( )) and (not is_preinclude) ) then
				cppAddDirectInclude( inctext )
			end if

			exit sub
		end if
	end if

	'' Get the normalized representation of the path, for use in hash tables
	'' etc. Otherwise foo.h from the root dir and ../foo.h from a subdir
	'' would be seen as different files.
	incfile = pathNormalize( pathMakeAbsolute( incfile ) )

	'' For display and -filterin/-filterout matching we use the version
	'' relative to curdir() though.
	var prettyfile = pathStripCurdir( incfile )
	var message = prettyfile

	var knownfile = cppLookupOrAppendKnownFile( incfile, is_rootfile, prettyfile )
	with( cpp.files[knownfile] )
		'' Before loading the include file content, do the #include guard optimization.
		'' If this file had an #include guard last time we saw it, and the guard symbol
		'' is now defined, then we don't need to bother loading (lex + tkInsert()...)
		'' the file at all now.
		if( .checked_guard and (.guard <> NULL) ) then
			'' Only load the file if the guard symbol isn't defined (anymore) now.
			if( cppIsMacroCurrentlyDefined( .guard ) ) then
				'' Skipping header due to include guard
				exit sub
			end if
		end if
	end with

	var filterout = cpp.files[knownfile].filterout
	if( filterout ) then
		message += " (filtered out)"

		'' We're expanding this #include statement, but the include content
		'' will be filtered out, so we have to keep the #include statement
		'' for the final binding, if it itself won't be filtered out.
		'' Pre-#includes should never be preserved though.
		if( (not cppWillBeFilteredOut( )) and (not is_preinclude) ) then
			cppAddDirectInclude( inctext )
		end if
	end if

	frogPrint( message )

	'' Push the #include file context
	cppPush( STATE_FILE, knownfile )

	'' Insert this helper token so we can identify the start of #included tokens later
	tkInsert( cpp.x, TK_BEGININCLUDE )
	if( filterout ) then
		tkAddFlags( cpp.x, cpp.x, TKFLAG_FILTEROUT )
	end if
	cpp.x += 1

	'' Insert EOL behind TK_BEGININCLUDE so we can can detect BOL there
	tkInsert( cpp.x, TK_EOL )
	tkSetRemove( cpp.x )
	cpp.x += 1

	'' Read the include file and insert its tokens
	var y = lexLoadC( cpp.x, sourcebufferFromFile( incfile, @location ) )

	'' Put TK_ENDINCLUDE behind the #include file content, so we can detect
	'' the included EOF and pop the #include context from the cpp.stack.
	tkInsert( y, TK_ENDINCLUDE )
	y += 1

	'' Insert EOL behind the TK_ENDINCLUDE so we can detect BOL there
	tkInsert( y, TK_EOL )
	tkSetRemove( y )
	y += 1

	'' Start parsing the #included content
	'' (starting behind the EOL inserted above)
	assert( tkGet( cpp.x - 2 ) = TK_BEGININCLUDE )
	assert( tkGet( cpp.x - 1 ) = TK_EOL )
	assert( y <= tkGetCount( ) )
	assert( tkGet( y - 2 ) = TK_ENDINCLUDE )
	assert( cpp.stack(cpp.level).state = STATE_FILE )
	cpp.stack(cpp.level).xbegininclude = cpp.x - 2

	if( cpp.files[knownfile].checked_guard = FALSE ) then
		'' Prepare for the include guard optimization:
		''  * Does the #include begin with the typical #include guard header?
		''     #ifndef FOO
		''     #define FOO
		var guard = hDetectIncludeGuardBegin( cpp.x )

		'' Store the guard (if any) for the file for the time being.
		'' We're tracking it in the cpp.stack, so if we find
		''  * that there is an #elif/#else,
		''  * or that we don't reach #include EOF after the #endif,
		'' then we can mark the include guard optimization as impossible
		'' by setting the guard to NULL.
		'' (see cppDisableIncludeGuardOptimization())
		cppKnownFileSetGuard( knownfile, guard )
	end if
end sub

private sub cppEndInclude( )
	assert( cpp.skiplevel = MAXSTACK )
	assert( cpp.level > 0 )
	if( cpp.stack(cpp.level).state >= STATE_IF ) then
		tkOops( cpp.x - 1, "missing #endif" )
	end if
	var begin = cpp.stack(cpp.level).xbegininclude
	cppPop( )

	assert( tkGet( begin ) = TK_BEGININCLUDE )
	assert( tkGet( cpp.x ) = TK_ENDINCLUDE )

	'' If the include content should be filtered out, mark all the included
	'' tokens accordingly, for the C parser later.
	if( tkGetFlags( begin ) and TKFLAG_FILTEROUT ) then
		tkAddFlags( begin + 1, cpp.x - 1, TKFLAG_FILTEROUT )
	end if

	'' Mark the TK_BEGININCLUDE/TK_ENDINCLUDE for removal, so they won't get in the
	'' way of C parsing (in case declarations cross #include/file boundaries).
	tkSetRemove( begin, begin )
	tkSetRemove( cpp.x, cpp.x )
	cpp.x += 1
end sub

'' DEFINE Identifier ['(' ParameterList ')'] Body Eol
private sub cppDefine( byval begin as integer, byref flags as integer )
	cpp.x += 1

	assert( cppSkipping( ) = FALSE )

	'' Identifier ['(' ParameterList ')']
	var macro = hDefineHead( cpp.x )

	'' Body
	var xbody = cpp.x
	cpp.x = hSkipToEol( cpp.x )

	'' Eol
	var xeol = cpp.x
	assert( tkGet( xeol ) = TK_EOL )
	cpp.x += 1

	var definfo = definfoNew( )
	definfo->xdefine = begin
	definfo->xbody = xbody
	definfo->xeol = xeol
	definfo->macro = macro

	'' Report conflicting #defines
	var prevdef = cppLookupMacro( macro->text )
	if( prevdef ) then
		if( definfoDefinesAreEqual( prevdef, definfo ) = FALSE ) then
			frogPrint( "conflicting #define " + *macro->text )
		end if
	end if

	cppDefineMacro( macro->text, definfo )

	'' Normally, we preserve #define directives (unlike the other CPP directives),
	'' thus no generic tkSetRemove() here. Unless the symbol was registed for removal.
	if( hashContains( @frog.idopt(OPT_REMOVEDEFINE), macro->text, hashHash( macro->text ) ) = FALSE ) then
		flags and= not TKFLAG_REMOVE
	end if
	flags or= TKFLAG_DEFINE
end sub

private sub cppUndef( )
	cpp.x += 1

	assert( cppSkipping( ) = FALSE )

	'' Identifier
	if( tkGet( cpp.x ) < TK_ID ) then
		tkExpect( cpp.x, TK_ID, "behind #undef" )
	end if
	var id = tkSpellId( cpp.x )
	cpp.x += 1

	cppUndefMacro( id )

	cppEol( )
end sub

private sub cppPragmaPushPopMacro( byval is_push as integer )
	cpp.x += 1

	var whatfor = iif( is_push, _
		@"for #pragma push_macro(""..."")", _
		@"for #pragma pop_macro(""..."")" )

	'' '('
	tkExpect( cpp.x, TK_LPAREN, whatfor )
	cpp.x += 1

	'' "..."
	tkExpect( cpp.x, TK_STRING, whatfor )
	var id = *tkGetText( cpp.x )
	cpp.x += 1

	'' ')'
	tkExpect( cpp.x, TK_RPAREN, whatfor )
	cpp.x += 1

	if( is_push ) then
		cppSaveMacro( id )
	else
		cppRestoreMacro( id )
	end if
end sub

private function cppPragma( byref flags as integer ) as integer
	select case( tkSpell( cpp.x ) )
	'' #pragma message("...")
	case "message"
		cpp.x += 1
		var whatfor = @"for #pragma message(""..."")"

		'' '('
		tkExpect( cpp.x, TK_LPAREN, whatfor )
		cpp.x += 1

		'' "..."
		tkExpect( cpp.x, TK_STRING, whatfor )
		cpp.x += 1

		'' ')'
		tkExpect( cpp.x, TK_RPAREN, whatfor )
		cpp.x += 1

	'' MSVC:
	'' #pragma comment(lib, "<library file name>")
	case "comment"
		cpp.x += 1

		'' '('
		tkExpect( cpp.x, TK_LPAREN, "for #pragma comment(...)" )
		cpp.x += 1

		select case( tkSpell( cpp.x ) )
		case "lib"
			cpp.x += 1

			'' ','
			tkExpect( cpp.x, TK_COMMA, "for #pragma comment(lib, ""..."")" )
			cpp.x += 1

			'' "..."
			tkExpect( cpp.x, TK_STRING, "for #pragma comment(lib, ""..."")" )
			cpp.x += 1

			'' Preserve the #pragma comment(lib, "...") for the C parser
			flags = 0

		case else
			exit function
		end select

		'' ')'
		tkExpect( cpp.x, TK_RPAREN, "for #pragma comment(...)" )
		cpp.x += 1

	case "GCC"
		cpp.x += 1

		select case( tkSpell( cpp.x ) )
		case "system_header", "push_options", "pop_options", "reset_options", "optimize", "target"
			'' Ignore
			cpp.x = hSkipToEol( cpp.x )

		case else
			exit function
		end select

	'' #pragma pack(N)
	'' #pragma pack()
	'' #pragma pack(push, N)
	'' #pragma pack(pop)
	case "pack"
		cpp.x += 1

		'' Just skip to EOL and let the C parser worry about checking
		'' the syntax
		cpp.x = hSkipToEol( cpp.x )

		'' Preserve the #pragma pack for the C parser
		flags = 0

	case "push_macro"
		cppPragmaPushPopMacro( TRUE )

	case "pop_macro"
		cppPragmaPushPopMacro( FALSE )

	case else
		exit function
	end select

	cppEol( )
	function = TRUE
end function

private sub cppDirective( )
	'' '#'
	var begin = cpp.x
	assert( tkGet( cpp.x ) = TK_HASH )
	cpp.x += 1

	var directivekw = tkGet( cpp.x )

	'' When skipping, only #if/#elif/#else/#endif directives are handled,
	'' anything else (even invalid directives) must be ignored.
	if( cppSkipping( ) ) then
		select case( directivekw )
		case KW_IF, KW_IFDEF, KW_IFNDEF, KW_ELIF, KW_ELSE, KW_ENDIF

		case else
			tkSetRemove( begin, cpp.x )
			cpp.x += 1
			exit sub
		end select
	end if

	var flags = TKFLAG_REMOVE

	select case( directivekw )
	case KW_IF
		cppIf( )

	case KW_IFDEF, KW_IFNDEF
		cppIfdef( directivekw )

	case KW_ELIF
		cppElseIf( )

	case KW_ELSE
		cppElse( )

	case KW_ENDIF
		cppEndIf( )

	case KW_INCLUDE
		cppInclude( begin )
		'' cppInclude() already marks the #include statement for removal.
		'' We can't do that here because then the TK_BEGININCLUDE would
		'' be marked too.
		flags = 0

	case KW_DEFINE
		cppDefine( begin, flags )

	case KW_UNDEF
		cppUndef( )

	case KW_PRAGMA
		cpp.x += 1
		if( cppPragma( flags ) = FALSE ) then
			tkOops( cpp.x, "unknown #pragma" )
		end if

	case KW_ERROR
		'' Not using the #error's text as error message,
		'' otherwise it would be mistaken for being generated by fbfrog.
		tkOops( cpp.x, "#error" )

	case KW_WARNING
		cpp.x += 1
		'' ditto
		print tkReport( cpp.x, "#warning" )
		cpp.x = hSkipToEol( cpp.x ) + 1

	case TK_EOL
		'' '#' followed by EOL (accepted by gcc/clang too)
		cpp.x += 1

	case else
		tkOops( cpp.x, "unknown PP directive" )
	end select

	if( flags ) then
		tkAddFlags( begin, cpp.x - 1, flags )
	end if
end sub

private sub cppNext( )
	select case( tkGet( cpp.x ) )
	case TK_ENDINCLUDE
		cppEndInclude( )
		exit sub

	'' '#'
	case TK_HASH
		'' Parse directive if at BOL and the '#' token isn't the result of a macro expansion
		'' We do this for every "toplevel" '#', before ever doing macro expansion behind it,
		'' so it should be safe to assume that if the '#' isn't coming from a macro expansion,
		'' the rest isn't either.
		if( tkIsEolOrEof( cpp.x - 1 ) and (tkGetExpansionLevel( cpp.x ) = 0) ) then
			cppDirective( )
			exit sub
		end if

	'' _Pragma("...")
	case KW__PRAGMA
		if( cppSkipping( ) = FALSE ) then
			var begin = cpp.x
			cpp.x += 1

			'' '('
			tkExpect( cpp.x, TK_LPAREN, "behind _Pragma" )
			cpp.x += 1

			'' StringLiteral
			tkExpect( cpp.x, TK_STRING, "inside _Pragma()" )
			var text = *tkGetText( cpp.x )
			cpp.x += 1

			'' ')'
			tkExpect( cpp.x, TK_RPAREN, "to close _Pragma" )
			cpp.x += 1

			'' Insert #pragma corresponding to the _Pragma(),
			'' while ensuring to have EOL in front of and behind it,
			'' mark the _Pragma() for removal, then we can parse the
			'' #pragma as usual.
			tkSetRemove( begin, cpp.x - 1 )
			var pragma = !"\n#pragma " + text
			if( tkGet( cpp.x ) <> TK_EOL ) then
				pragma += !"\n"
			end if
			lexLoadC( cpp.x, sourcebufferFromZstring( "_Pragma(" + text + ")", pragma, tkGetLocation( begin ) ) )
			exit sub
		end if

	'' Identifier/keyword? Check whether it needs to be macro-expanded
	case is >= TK_ID
		if( cppSkipping( ) = FALSE ) then
			hMaybeExpandMacro( cpp.x, FALSE )
			cpp.x += 1
			exit sub
		end if

	'' Remove standalone EOLs, so the C parser doesn't have to handle them
	case TK_EOL
		tkSetRemove( cpp.x )
		cpp.x += 1
		exit sub
	end select

	'' Some token that doesn't matter to the CPP
	if( cppSkipping( ) ) then
		tkSetRemove( cpp.x )
	end if
	cpp.x += 1
end sub

sub cppMain( )
	while( tkGet( cpp.x ) <> TK_EOF )
		cppNext( )
	wend
end sub

sub hMoveDefinesOutOfConstructs( )
	var x = 0
	do
		'' Skip #define(s) at begin of construct
		while( tkGetFlags( x ) and TKFLAG_DEFINE )
			x += 1
		wend

		if( tkGet( x ) = TK_EOF ) then
			exit do
		end if

		var nxt = hSkipConstruct( x, TRUE )

		'' Exclude #define(s) at end of construct from the construct
		while( tkGetFlags( nxt - 1 ) and TKFLAG_DEFINE )
			nxt -= 1
		wend
		assert( x < nxt )

		'' Handle #defines inside this construct: Move them to the end
		'' and exclude them from the construct.
		var writepos = nxt
		while( x < nxt )
			if( tkGetFlags( x ) and TKFLAG_DEFINE ) then
				'' Collect all #defines in a row
				var y = x
				while( tkGetFlags( y + 1 ) and TKFLAG_DEFINE )
					y += 1
				wend
				assert( tkGet( y ) = TK_EOL )
				assert( y < nxt )

				'' Move from middle to the end (but behind previously moved
				'' #defines, to preserve their order)
				tkCopy( writepos, x, y, -1 )
				tkRemove( x, y )

				'' Update end-of-construct position as we're moving
				'' #defines out of the current construct
				nxt -= y - x + 1
			else
				x += 1
			end if
		wend
	loop
end sub

sub hOnlyFilterOutWholeConstructs( )
	var x = 0
	while( tkGet( x ) <> TK_EOF )
		var nxt = hSkipConstruct( x, FALSE )

		'' Count occurences of TKFLAG_FILTEROUT in this construct
		var filterouts = 0
		for i as integer = x to nxt - 1
			if( tkGetFlags( i ) and TKFLAG_FILTEROUT ) then
				filterouts += 1
			end if
		next

		'' If only some but not all tokens in the construct are marked
		'' with TKFLAG_FILTEROUT, then we better forget about that,
		'' otherwise we could end up deleting a partial construct...
		if( (filterouts > 0) and (filterouts < (nxt - x)) ) then
			tkUnsetFilterOut( x, nxt - 1 )
		end if

		x = nxt
	wend
end sub
