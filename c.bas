''
'' C parsing
''
'' This parses the content of the tk buffer and builds an AST.
'' We have ...
''  * A recursive declaration parser that can handle multiple declarations in
''    the same statement and nested declarations such as function pointers
''    returning function pointers etc.
''  * An expression parser used for variable/parameter initializers, enum
''    constants, #define bodies.
''  * A data type parser: base types in declarations including struct/union/enum
''    with or without body, type casts, sizeof().
''
'' The parser is able to recover from parsing errors, such that it can continue
'' parsing the next construct if parsing the current one failed. The bad
'' constructs are preserved in the AST in form of ASTCLASS_UNKNOWN nodes.
''
'' Even though the parser can handle a lot, it's still incomplete, and it will
'' never be able to handle all possible #define bodies, so the error recovery
'' will never really be obsolete, unless the user is asked to restart fbfrog
'' everytime, while adding certain command line options to tell fbfrog to remove
'' the bad construct before trying to parse it the next time. But why bother the
'' user with that, when it can be done automatically? It makes the code here
'' more ugly, but that's not really an excuse.
''
'' Rules:
''  * All loops during cConstruct() must check parseok
''

#include once "fbfrog.bi"

declare function cExpression( ) as ASTNODE ptr
declare function cExpressionOrInitializer( ) as ASTNODE ptr
declare function cDataType( ) as ASTNODE ptr
declare sub cDeclaration( byval astclass as integer, byval gccattribs as integer )
declare function cScope( byval proc as ASTNODE ptr ) as ASTNODE ptr
declare sub cConstruct( )
declare sub cBody( )

const BLOCKSTACKLEN = 16

namespace c
	'' We track the C namespaces so we can detect duplicate declarations
	'' (e.g. redundant procedure declarations are allowed in C), and the
	'' FB namespaces in order to detect symbol id conflicts.
	''
	'' block stack:
	''  The toplevel context and any '{...}' block is pushed here. Holds a
	''  pointer to the context AST node, so we know where to add the nodes
	''  for new declarations.
	''
	'' namespace stack:
	''  Any blocks holding "ordinary identifiers", i.e. vars/procs/typedefs/
	''  enumconsts at toplevel, params inside procs, fields inside structs.
	''  This is separate from the block stack because not all '{...}' blocks
	''  start new namespaces (i.e. enums and anonymous nested structs).
	''
	'' scope stack:
	''  Every scope is a namespace, but struct/union bodies are only
	''  namespaces and not scopes.
	''
	'' This distinction between namespace & scope is only made to avoid
	'' scoping new tags inside the '{...}' bodies of other tags. Namespaces
	'' only capture names, but not tags.

	type BLOCKNODE
		context as ASTNODE ptr
		start_namespace as integer
		start_scope as integer
	end type

	type NAMESPACENODE
		owner as ASTNODE ptr

		dummyidcounter as integer

		'' C "ordinary identifier" namespace (typedefs/vars/procs/fields/...)
		cnames as THASH

		'' FB namespaces
		fbnames as THASH
		fbtypes as THASH

		'' Nodes for the renamelist, from this level, to be propagated upwards
		renamelist as ASTNODE ptr
	end type

	type SCOPENODE
		owner as ASTNODE ptr

		'' C tag namespace
		ctags as THASH
	end type

	dim shared blocks(0 to BLOCKSTACKLEN-1) as BLOCKNODE
	dim shared namespaces(0 to BLOCKSTACKLEN-1) as NAMESPACENODE
	dim shared scopes(0 to BLOCKSTACKLEN-1) as SCOPENODE
	dim shared as integer blocklevel, namespacelevel, scopelevel

	'' Global hash table for #defines since they're not scoped
	dim shared cdefines  as THASH  '' Original C #define identifiers
	dim shared fbdefines as THASH  '' Ucase'd #defines, and reserved ids from cAddReservedId()

	'' #pragma pack stack
	namespace pragmapack
		const MAXLEVEL = 128
		dim shared stack(0 to MAXLEVEL-1) as integer
		dim shared level as integer
	end namespace

	'' x = index of current token
	dim shared as integer x, parseok, filterout
	dim shared parentdefine as ASTNODE ptr

	type DEFBODYNODE
		'' Token positions:
		xbegin		as integer  '' Begin of the whole #define directive
		xbodybegin	as integer  '' Begin of the #define's body
		n		as ASTNODE ptr  '' #define node
	end type
	dim shared defbodies as DEFBODYNODE ptr
	dim shared as integer defbodycount, defbodyroom

	'' Tags from the toplevel scope that were used before being defined
	'' (those for which we have to add forward decls)
	dim shared tags as ASTNODE ptr ptr
	dim shared tagcount as integer
	dim shared taghash as THASH
end namespace

private function cMatch( byval tk as integer ) as integer
	if( tkGet( c.x ) = tk ) then
		c.x += 1
		function = TRUE
	end if
end function

private sub cError( byval message as zstring ptr )
	if( c.parseok ) then
		c.parseok = FALSE
		if( frog.verbose ) then
			print tkReport( c.x, message )
		end if
	end if
end sub

private sub cOops( byval message as zstring ptr )
	print tkReport( c.x, message )
	end 1
end sub

private sub cExpectMatch( byval tk as integer, byval message as zstring ptr )
	if( tkGet( c.x ) = tk ) then
		c.x += 1
	elseif( c.parseok ) then
		c.parseok = FALSE
		if( frog.verbose ) then
			print tkReport( c.x, tkMakeExpectedMessage( c.x, tkInfoPretty( tk ) + " " + *message ) )
		end if
	end if
end sub

private sub cResetPragmaPack( )
	c.pragmapack.stack(c.pragmapack.level) = 0
end sub

private sub cAddRenameListEntry( byval entry as ASTNODE ptr )
	with( c.namespaces(c.namespacelevel) )
		if( .renamelist = NULL ) then
			dim title as string
			if( c.namespacelevel = 0 ) then
				title = "The following symbols have been renamed:"
			else
				title = "inside " + astDumpPrettyDecl( .owner ) + ":"
			end if
			.renamelist = astNew( ASTCLASS_RENAMELIST, title )
		end if
		astAppend( .renamelist, entry )
	end with
end sub

private sub cPush( byval context as ASTNODE ptr, byval start_namespace as integer, byval start_scope as integer )
	c.blocklevel += 1
	if( c.blocklevel >= BLOCKSTACKLEN ) then
		cOops( "too much nesting, internal stack too small" )
	end if
	with( c.blocks(c.blocklevel) )
		.context = context
		.start_namespace = start_namespace
		.start_scope = start_scope
	end with

	var namehashtbsize = iif( c.blocklevel = 0, 13, 5 )
	var typehashtbsize = iif( c.blocklevel = 0,  7, 3 )

	if( start_namespace ) then
		c.namespacelevel += 1
		with( c.namespaces(c.namespacelevel) )
			.owner = context
			.dummyidcounter = 0
			hashInit( @.cnames , namehashtbsize, FALSE )
			hashInit( @.fbnames, namehashtbsize, TRUE )
			hashInit( @.fbtypes, typehashtbsize, TRUE )
			.renamelist = NULL
		end with
	end if

	if( start_scope ) then
		c.scopelevel += 1
		with( c.scopes(c.scopelevel) )
			.owner = context
			hashInit( @.ctags, typehashtbsize, FALSE )
		end with
	end if
end sub

private sub cPop( )
	dim nestedrenamelist as ASTNODE ptr

	'' Pop scope if one was started at this block level
	if( c.blocks(c.blocklevel).start_scope ) then
		with( c.scopes(c.scopelevel) )
			hashEnd( @.ctags )
		end with
		c.scopelevel -= 1
	end if

	'' Same for namespace
	if( c.blocks(c.blocklevel).start_namespace ) then
		with( c.namespaces(c.namespacelevel) )
			hashEnd( @.cnames  )
			hashEnd( @.fbnames )
			hashEnd( @.fbtypes )
			nestedrenamelist = .renamelist
		end with
		c.namespacelevel -= 1
	end if

	c.blocklevel -= 1

	'' Propagate renamelist upwards, if any
	if( nestedrenamelist ) then
		if( c.namespacelevel >= 0 ) then
			'' Add renamelist from the nested scope to the parent
			cAddRenameListEntry( nestedrenamelist )
		else
			'' Popping the toplevel scope; store the final renamelist
			api->renamelist = nestedrenamelist
		end if
	end if
end sub

private function hSolveTypedefOutIfWanted( byval n as ASTNODE ptr ) as ASTNODE ptr
	if( n ) then
		if( n->class = ASTCLASS_TYPEDEF ) then
			if( n->attrib and ASTATTRIB_SOLVEOUT ) then
				'' Currently the "solve typedef out" functionality is only used
				'' for typedefs which alias structs
				assert( n->dtype = TYPE_UDT )
				assert( (n->subtype->class = ASTCLASS_STRUCT) or _
					(n->subtype->class = ASTCLASS_UNION) or _
					(n->subtype->class = ASTCLASS_ENUM) )
				n = n->subtype
			end if
		end if
	end if
	function = n
end function

private function cLocalNameLookup( byval id as zstring ptr ) as ASTNODE ptr
	function = hashLookupDataOrNull( @c.namespaces(c.namespacelevel).cnames, id )
end function

private function cFullNameLookup( byval id as zstring ptr ) as ASTNODE ptr
	for i as integer = c.namespacelevel to 0 step -1
		dim as ASTNODE ptr n = hashLookupDataOrNull( @c.namespaces(i).cnames, id )
		if( n ) then
			return n
		end if
	next
end function

private function cLocalTagLookup( byval id as zstring ptr ) as ASTNODE ptr
	function = hashLookupDataOrNull( @c.scopes(c.scopelevel).ctags, id )
end function

private function cFullTagLookup( byval id as zstring ptr ) as ASTNODE ptr
	for i as integer = c.scopelevel to 0 step -1
		dim as ASTNODE ptr n = hashLookupDataOrNull( @c.scopes(i).ctags, id )
		if( n ) then
			return n
		end if
	next
end function

private function cDefineLookup( byval id as zstring ptr ) as ASTNODE ptr
	function = hashLookupDataOrNull( @c.cdefines, id )
end function

private sub cAppendNode( byval n as ASTNODE ptr )
	if( c.parseok ) then
		astAppend( c.blocks(c.blocklevel).context, n )
	end if
end sub

private sub hMaybeOverrideSymbolId( byval opt as integer, byval n as ASTNODE ptr )
	dim as ASTNODE ptr renameinfo = hashLookupDataOrNull( @frog.renameopt(opt), n->text )
	if( renameinfo ) then
		assert( *n->text = *renameinfo->alias )
		astSetText( n, renameinfo->text )
		n->attrib or= ASTATTRIB_NAMEOVERRIDDEN
	end if
end sub

private function hHaveConflict _
	( _
		byval n as ASTNODE ptr, _
		byref ucaseid as string, _
		byval ucaseidhash as ulong, _
		byref existing as ASTNODE ptr _
	) as integer

	dim item as THASHITEM ptr

	#macro hCheck( hashtb )
		item = hashLookup( hashtb, ucaseid, ucaseidhash )
		if( item->s ) then
			existing = item->data  '' NULL in case of FB keyword, non-NULL for other symbols
			return TRUE
		end if
	#endmacro

	select case( n->class )
	case ASTCLASS_PPDEFINE
		hCheck( @fbkeywordhash )
		hCheck( @c.fbdefines )
		hCheck( @c.namespaces(0).fbtypes )
		hCheck( @c.namespaces(0).fbnames )

	case ASTCLASS_STRUCT, ASTCLASS_UNION, ASTCLASS_ENUM, ASTCLASS_TYPEDEF
		hCheck( @fbkeywordhash )
		hCheck( @c.fbdefines )
		hCheck( @c.namespaces(c.namespacelevel).fbtypes )

	case ASTCLASS_FIELD
		hCheck( @c.fbdefines )
		hCheck( @c.namespaces(c.namespacelevel).fbnames )

		'' Fields can be named after FB keywords, except for '_'
		'' Since we're not checking the keyword hash table here,
		'' we must check for '_' manually.
		if( ucaseid = "_" ) then
			return TRUE
		end if

	case else
		'' proc/var/enumconst/param
		hCheck( @fbkeywordhash )
		hCheck( @c.fbdefines )
		hCheck( @c.namespaces(c.namespacelevel).fbnames )
	end select

	function = FALSE
end function

private function hPreferRenaming( byval n as ASTNODE ptr ) as integer
	select case( n->class )
	case ASTCLASS_PPDEFINE
		function = (c.namespacelevel = 0)
	case ASTCLASS_ENUMCONST
		function = TRUE
	end select
end function

'' Decide which symbol to rename: the existing one, or the new one.
private function hShouldRenameExistingSymbol( byval existing as ASTNODE ptr, byval n as ASTNODE ptr ) as integer
	'' Conflicting with FB keyword?
	if( existing = NULL ) then return FALSE

	'' If conflicting with an existing symbol that was already renamed,
	'' then we just rename the existing one again. We don't want renames
	'' to cause more renames.
	if( existing->attrib and ASTATTRIB_RENAMED ) then return TRUE

	'' Parameters are the easiest to rename because renaming them doesn't
	'' make a difference (in prototypes)
	if(        n->class = ASTCLASS_PARAM ) then return FALSE
	if( existing->class = ASTCLASS_PARAM ) then return TRUE

	'' Prefer renaming #defines/constants over others
	if( hPreferRenaming( n        ) ) then return FALSE
	if( hPreferRenaming( existing ) ) then return TRUE

	'' Fallback to renaming the symbol that appeared later
	function = FALSE
end function

'' Rename a symbol by appending _ underscores to its id, as long as needed to
'' find an id that's not yet used in the namespace. (otherwise renaming could
'' cause more conflicts)
private sub hRenameSymbol _
	( _
		byval n as ASTNODE ptr, _
		byref ucaseid as string, _
		byref ucaseidhash as ulong, _
		byval other as ASTNODE ptr _
	)

	assert( n->text )
	var newid = *n->text

	do
		newid += "_"
		ucaseid = ucase( newid, 1 )
		ucaseidhash = hashHash( ucaseid )
	loop while( hHaveConflict( n, ucaseid, ucaseidhash, NULL ) )

	'' Add renamelist entry
	''  * unless this declaration should be filtered out
	''  * unless one was added already
	''  * unless it's a parameter, because those aren't interesting
	if( ((n->attrib and (ASTATTRIB_FILTEROUT or ASTATTRIB_RENAMED)) = 0) and _
	    (n->class <> ASTCLASS_PARAM) ) then
		var renamelistentry = astNew( ASTCLASS_RENAMELISTENTRY )
		renamelistentry->expr = astNewSYM( n )
		cAddRenameListEntry( renamelistentry )
	end if

	astRenameSymbol( n, newid )
	n->attrib or= ASTATTRIB_RENAMED
end sub

private function hGetFbHashTb( byval n as ASTNODE ptr ) as THASH ptr
	select case( n->class )
	case ASTCLASS_PPDEFINE
		function = @c.fbdefines
	case ASTCLASS_STRUCT, ASTCLASS_UNION, ASTCLASS_ENUM, ASTCLASS_TYPEDEF
		function = @c.namespaces(c.namespacelevel).fbtypes
	case else
		function = @c.namespaces(c.namespacelevel).fbnames
	end select
end function

private sub cAddCHashTbEntry( byval n as ASTNODE ptr )
	'' Add the symbol to the proper C namespace
	dim chashtb as THASH ptr
	select case( n->class )
	case ASTCLASS_PPDEFINE
		chashtb = @c.cdefines
	case ASTCLASS_STRUCT, ASTCLASS_UNION, ASTCLASS_ENUM
		chashtb = @c.scopes(c.scopelevel).ctags
	case else
		chashtb = @c.namespaces(c.namespacelevel).cnames
	end select

	'' If it was renamed already, add it as the old name (because that's
	'' what we have to be able to look up when parsing the C code)
	hashAddOverwrite( chashtb, astGetOrigId( n ), n )
end sub

private sub cAddSymbol( byval n as ASTNODE ptr )
	cAddCHashTbEntry( n )

	'' Override the symbol's original id if a rename was requested on the command line
	'' We do this after adding it to the hashtb under its original name, because then
	'' we'll still be able to lookup references to the old name, and they will automatically
	'' reference the symbol with its new name.
	select case( n->class )
	case ASTCLASS_TYPEDEF
		hMaybeOverrideSymbolId( OPT_RENAMETYPEDEF, n )
	case ASTCLASS_STRUCT, ASTCLASS_UNION, ASTCLASS_ENUM
		hMaybeOverrideSymbolId( OPT_RENAMETAG, n )
	end select

	if( c.filterout ) then
		n->attrib or= ASTATTRIB_FILTEROUT
	end if

	if( frog.syntaxonly or _
	    ((n->attrib and ASTATTRIB_SOLVEOUT) <> 0) or _
	    (n->class = ASTCLASS_TEXT) ) then
		'' No auto-renaming for this
		exit sub
	end if

	'' Check for -dontrenamefield <id>
	if( n->class = ASTCLASS_FIELD ) then
		if( hashContains( @frog.idopt(OPT_DONTRENAMEFIELD), n->text, hashHash( n->text ) ) ) then
			exit sub
		end if
	end if

	'' Check whether this symbol conflicts with an existing one (from FB's
	'' point of view) and rename one of them if needed.
	dim as ASTNODE ptr existing, torename, other
	var ucaseid = ucase( *n->text, 1 )
	var ucaseidhash = hashHash( ucaseid )
	if( hHaveConflict( n, ucaseid, ucaseidhash, existing ) ) then
		var rename_existing = hShouldRenameExistingSymbol( existing, n )

		if( ((n->attrib and ASTATTRIB_NAMEOVERRIDDEN) <> 0) and (not rename_existing) ) then
			'' Don't change id given by the user - the user is always right.
			'' Rename the existing symbol instead, if possible.
			'' If both symbols were renamed by the user (or the existing one is a keyword,
			'' which we can't rename anyways), and they conflict - do nothing.
			if( existing = NULL ) then
				exit sub
			end if
			if( existing->attrib and ASTATTRIB_NAMEOVERRIDDEN ) then
				exit sub
			end if
			rename_existing = TRUE
		end if

		if( rename_existing ) then
			torename = existing
			other = n
		else
			torename = n
			other = existing
		end if

		dim as string torenameucaseid
		dim as ulong torenameucaseidhash
		hRenameSymbol( torename, torenameucaseid, torenameucaseidhash, other )

		'' Renamed existing symbol instead of this one?
		if( torename = existing ) then
			'' Re-add the existing symbol to its namespace under its new name
			hashAddOverwrite( hGetFbHashTb( torename ), torenameucaseid, torename )
		else
			ucaseid = torenameucaseid
			ucaseidhash = torenameucaseidhash
		end if
	end if

	'' Add the symbol to the proper FB namespace
	'' (may need to overwrite existing symbol if that got renamed instead of
	'' this one)
	hashAddOverwrite( hGetFbHashTb( n ), ucaseid, n )
end sub

private sub cAddToplevelSymbol( byval n as ASTNODE ptr )
	var old_blocklevel = c.blocklevel
	var old_namespacelevel = c.namespacelevel
	var old_scopelevel = c.scopelevel

	c.blocklevel = 0
	c.namespacelevel = 0
	c.scopelevel = 0

	cAddSymbol( n )

	c.blocklevel = old_blocklevel
	c.namespacelevel = old_namespacelevel
	c.scopelevel = old_scopelevel
end sub

private sub cAddTag( byval tag as ASTNODE ptr )
	if( hashContains( @c.taghash, tag->text, hashHash( tag->text ) ) ) then
		exit sub
	end if
	c.tagcount += 1
	c.tags = reallocate( c.tags, c.tagcount * sizeof( *c.tags ) )
	c.tags[c.tagcount-1] = tag
	hashAddOverwrite( @c.taghash, tag->text, NULL )
end sub

private function cIsTypedef( byval id as zstring ptr ) as integer
	'' 1. Check C parser's symbol table
	var n = cFullNameLookup( id )
	if( n ) then
		'' Known symbol; we can tell whether it's a typedef or not
		return (n->class = ASTCLASS_TYPEDEF)
	end if

	'' 2. Check -typedefhint options
	function = hashContains( @frog.idopt(OPT_TYPEDEFHINT), id, hashHash( id ) )
end function

private function cIdentifierIsMacroParam( byval id as zstring ptr ) as integer
	if( c.parentdefine ) then
		function = (astLookupMacroParam( c.parentdefine, id ) >= 0)
	else
		function = FALSE
	end if
end function

sub cInit( )
	'' AST root node
	api->ast = astNew( ASTCLASS_GROUP )

	'' Init toplevel scope
	c.blocklevel = -1
	c.namespacelevel = -1
	c.scopelevel = -1
	cPush( api->ast, TRUE, TRUE )
	hashInit( @c.cdefines, 8, FALSE )
	hashInit( @c.fbdefines, 8, TRUE )

	'' Initially no packing
	c.pragmapack.level = 0
	cResetPragmaPack( )

	c.x = 0
	c.parseok = TRUE
	c.parentdefine = NULL

	c.defbodies = NULL
	c.defbodycount = 0
	c.defbodyroom = 0

	c.tags = NULL
	c.tagcount = 0
	hashInit( @c.taghash, 4, FALSE )
end sub

''
'' Add a forward reference for the given tag, and update all references to the
'' tag to point to the new forward typedef.
''
'' We can do this fairly easily by turning the tag node itself into the forward
'' typedef. This way all references will stay valid and will automatically point
'' to the forward decl. It can then be moved to the top of the code.
''
'' In case the tag node represented a tag body in the code, we can insert a new
'' node in the tag's old place. Then the new node will represent the tag body.
'' (since the old tag node is busy being the forward typedef)
''
'' For completeness' sake, we need to create a new tag node anyways, to
'' represent the forward reference (the dtype/subtype of the forward decl),
'' if there is no tag body.
''
private sub hAddFwdDecl( byval parent as ASTNODE ptr, byval oldtag as ASTNODE ptr )
	var newtag = astCloneNode( oldtag )
	astMoveChildren( newtag, oldtag )

	'' New name for the forward reference
	astSetText( newtag, *newtag->text + "_" )

	'' If there's a body, insert it in front of the old node
	if( oldtag->attrib and ASTATTRIB_BODYDEFINED ) then
		astInsert( parent, newtag, oldtag )

		'' Unlink old node
		astRemove( parent, oldtag )
	end if

	'' Add the old node to the top
	astPrepend( parent, oldtag )

	'' Turn it into the wanted forward typedef
	oldtag->class = ASTCLASS_TYPEDEF
	oldtag->dtype = TYPE_UDT
	oldtag->subtype = newtag
end sub

sub cEnd( )
	'' Add a forward decl for any tags that were used before being defined
	'' (looping backwards because the forward decls will be prepended; this
	'' way they'll end up in the proper order)
	for i as integer = c.tagcount - 1 to 0 step -1
		var tag = c.tags[i]
		if( ((tag->attrib and ASTATTRIB_USEBEFOREDEF) <> 0) and _
		    ((tag->attrib and ASTATTRIB_DONTADDFWDREF) = 0) ) then
			hAddFwdDecl( api->ast, tag )
		end if
	next

	'' Cleanup toplevel scope
	cPop( )
	assert( c.blocklevel = -1 )
	assert( c.namespacelevel = -1 )
	assert( c.scopelevel = -1 )

	hashEnd( @c.cdefines )
	hashEnd( @c.fbdefines )

	deallocate( c.defbodies )

	deallocate( c.tags )
	hashEnd( @c.taghash )
end sub

sub cAddReservedId( byval id as zstring ptr )
	'' ucase it as done by cAddSymbol()'s id conflict checking
	hashAddOverwrite( @c.fbdefines, ucase( *id, 1 ), NULL )
end sub

private sub cAddDefBody( byval xbegin as integer, byval xbodybegin as integer, byval n as ASTNODE ptr )
	if( c.defbodyroom = c.defbodycount ) then
		if( c.defbodyroom = 0 ) then
			c.defbodyroom = 512
		else
			c.defbodyroom *= 2
		end if
		c.defbodies = reallocate( c.defbodies, c.defbodyroom * sizeof( *c.defbodies ) )
	end if
	with( c.defbodies[c.defbodycount] )
		.xbegin = xbegin
		.xbodybegin = xbodybegin
		.n = n
	end with
	c.defbodycount += 1
end sub

''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''

'' ("..." | #id)*
private function cStringLiteralSequence( ) as ASTNODE ptr
	dim as ASTNODE ptr a

	while( c.parseok )
		dim as ASTNODE ptr s

		select case( tkGet( c.x ) )
		case TK_STRING
			s = astNew( ASTCLASS_STRING, tkGetText( c.x ) )
			astSetType( s, TYPE_ZSTRING, NULL )

		case TK_WSTRING
			s = astNew( ASTCLASS_STRING, tkGetText( c.x ) )
			astSetType( s, TYPE_WSTRING, NULL )

		'' '#' stringify operator
		case TK_HASH
			'' #id?
			if( tkGet( c.x + 1 ) <> TK_ID ) then
				exit while
			end if
			c.x += 1

			s = astNew( ASTCLASS_STRINGIFY, astNewTEXT( tkGetText( c.x ) ) )

		case else
			exit while
		end select

		if( a = NULL ) then
			a = s
		else
			a = astNew( ASTCLASS_STRCAT, a, s )
		end if

		c.x += 1
	wend

	function = a
end function

''
'' Trying to disambiguate between DataType and Expression: Even without being a
'' full C compiler, and even without seeing the whole C source (system #includes
'' etc), good guesses can be made.
''
'' If it starts with a data type keyword, and isn't inside a macro where that's
'' a macro parameter, then it must be a data type, because it couldn't appear in
'' an expression.
''
'' Of course that's an unsafe assumption because any identifier could have been
'' re-#defined to something different than what fbfrog assumes, in #include
'' files that fbfrog doesn't even parse, etc... but for common typedefs such as
'' size_t that shouldn't be a problem in practice.
''
'' If there's just an identifier then it could be a typedef but we can't be
'' sure. Finding out whether it is a typedef would require checking all previous
'' declarations in this file and in #includes, that's not possible currently
'' because #includes aren't always merged in.
''
'' Note: fbfrog could show a warning then making such an unsafe assumption,
'' but on the other hand, that's rather pointless because without seeing
'' all #defines, no C code is safe to parse. If int/void etc. are re-#defined
'' without fbfrog knowing then the for example the declaration parser would
'' make the same mistake, but it doesn't show any warning. That would be crazy
'' to do for every re-#definable keyword...
''
private function hIsDataType( byval y as integer ) as integer
	var is_type = FALSE

	select case( tkGet( y ) )
	case KW_SIGNED, KW_UNSIGNED, KW_CONST, KW_SHORT, KW_LONG, _
	     KW_ENUM, KW_STRUCT, KW_UNION, _
	     KW_VOID, KW_CHAR, KW_FLOAT, KW_DOUBLE, KW_INT
		is_type = not cIdentifierIsMacroParam( tkSpellId( y ) )
	case TK_ID
		var id = tkSpellId( y )
		if( (extradatatypesLookup( id ) <> TYPE_NONE) or cIsTypedef( id ) ) then
			is_type = not cIdentifierIsMacroParam( id )
		end if
	end select

	function = is_type
end function

private function hIsDataTypeOrAttribute( byval y as integer ) as integer
	select case( tkGet( y ) )
	case KW___ATTRIBUTE__
		function = not cIdentifierIsMacroParam( tkSpellId( y ) )
	case else
		function = hIsDataType( y )
	end select
end function

private function cNumberLiteral( ) as ASTNODE ptr
	dim errmsg as string
	var n = hNumberLiteral( c.x, FALSE, errmsg, c.filterout )
	if( n = NULL ) then
		cError( errmsg )
		n = astNew( ASTCLASS_CONSTI, "0" )
		astSetType( n, TYPE_LONG, NULL )
	end if
	c.x += 1
	function = n
end function

private function hLookupOrAddName( byval id as zstring ptr ) as ASTNODE ptr
	dim n as ASTNODE ptr

	if( c.parentdefine ) then
		'' Macro param? (inside macro body only)
		if( astLookupMacroParam( c.parentdefine, id, n ) >= 0 ) then
			return n
		end if
	end if

	'' #define?
	'' Macros come before procs/vars, before they're expanded first,
	'' except that macros can't be recursive, so inside a macro's
	'' body we can't lookup that macro itself.
	n = cDefineLookup( id )
	if( n ) then
		if( c.parentdefine <> n ) then
			return n
		end if
	end if

	'' Known typedef/proc/var?
	n = hSolveTypedefOutIfWanted( cFullNameLookup( id ) )
	if( n ) then
		return n
	end if

	'' Add as unknown id
	n = astNewTEXT( id )
	cAddSymbol( n )
	function = n
end function

'' C expression parser based on precedence climbing
private function hExpression( byval level as integer ) as ASTNODE ptr
	'' Unary prefix operators
	var op = -1
	select case( tkGet( c.x ) )
	case TK_EXCL   : op = ASTCLASS_CLOGNOT   '' !
	case TK_TILDE  : op = ASTCLASS_NOT       '' ~
	case TK_MINUS  : op = ASTCLASS_NEGATE    '' -
	case TK_PLUS   : op = ASTCLASS_UNARYPLUS '' +
	case TK_AMP    : op = ASTCLASS_ADDROF    '' &
	case TK_STAR   : op = ASTCLASS_DEREF     '' *
	end select

	dim as ASTNODE ptr a
	if( op >= 0 ) then
		c.x += 1
		a = astNew( op, hExpression( cprecedence(op) ) )
	else
		'' Atoms
		select case( tkGet( c.x ) )

		''     '(' Expression ')'
		'' or: '(' DataType ')' Expression
		case TK_LPAREN
			'' '('
			c.x += 1

			var is_cast = hIsDataTypeOrAttribute( c.x )

			'' Find the ')' and check the token behind it, in some cases
			'' we can tell that it probably isn't a cast.
			var closingparen = hFindClosingParen( c.x - 1, (c.parentdefine <> NULL), FALSE )
			select case( tkGet( closingparen + 1 ) )
			case TK_RPAREN, TK_EOF, TK_EOL
				is_cast = FALSE
			end select

			'' Something of the form '(id*)' or just in general a
			'' '*' in front of the closing ')'? It most likely is a pointer cast.
			is_cast or= (tkGet( closingparen - 1 ) = TK_STAR)

			if( is_cast ) then
				'' DataType
				var t = cDataType( )

				'' ')'
				cExpectMatch( TK_RPAREN, "behind the data type" )

				'' Expression
				a = hExpression( cprecedence(ASTCLASS_CAST) )

				assert( t->class = ASTCLASS_DATATYPE )
				t->class = ASTCLASS_CAST
				astAppend( t, a )
				a = t
			else
				'' Expression
				a = hExpression( 0 )

				'' ')'
				cExpectMatch( TK_RPAREN, "to close '(...)' parenthesized expression" )

				if( a->class = ASTCLASS_SYM ) then
					if( a->expr->class = ASTCLASS_MACROPARAM ) then
						a->attrib or= ASTATTRIB_PARENTHESIZEDMACROPARAM
					end if
				end if
			end if

		case TK_NUMBER
			a = cNumberLiteral( )

		case TK_STRING, TK_WSTRING, TK_HASH
			a = cStringLiteralSequence( )

		case TK_CHAR
			a = astNew( ASTCLASS_CHAR, tkGetText( c.x ) )
			astSetType( a, TYPE_ZSTRING, NULL )
			c.x += 1

		case TK_WCHAR
			a = astNew( ASTCLASS_CHAR, tkGetText( c.x ) )
			astSetType( a, TYPE_WSTRING, NULL )
			c.x += 1

		'' Identifier ['(' [CallArguments] ')']
		case TK_ID
			a = astNewSYM( hLookupOrAddName( tkSpellId( c.x ) ) )
			c.x += 1

			select case( tkGet( c.x ) )
			'' '('?
			case TK_LPAREN
				var callnode = astNew( ASTCLASS_CALL )
				callnode->expr = a
				a = callnode
				c.x += 1

				'' [CallArguments]
				if( tkGet( c.x ) <> TK_RPAREN ) then
					'' Expression (',' Expression)*
					do
						astAppend( a, cExpression( ) )

						'' ','?
					loop while( cMatch( TK_COMMA ) and c.parseok )
				end if

				'' ')'?
				cExpectMatch( TK_RPAREN, "to close call argument list" )

			'' '##'?
			case TK_HASHHASH
				var t = astNew( ASTCLASS_PPMERGE )
				astAppend( t, a )
				a = t
				c.x += 1

				'' Identifier ('##' Identifier)*
				do
					'' Identifier?
					if( tkGet( c.x ) = TK_ID ) then
						astAppend( a, astNewSYM( hLookupOrAddName( tkSpellId( c.x ) ) ) )
						c.x += 1
					else
						cError( "expected identifier as operand of '##' PP merge operator" + tkButFound( c.x ) )
					end if

					'' '##'?
				loop while( cMatch( TK_HASHHASH ) and c.parseok )

			end select

		'' SIZEOF Expression
		'' SIZEOF '(' DataType ')'
		case KW_SIZEOF
			c.x += 1

			'' ('(' DataType)?
			if( (tkGet( c.x ) = TK_LPAREN) andalso hIsDataTypeOrAttribute( c.x + 1 ) ) then
				'' '('
				c.x += 1

				'' DataType
				a = cDataType( )

				'' ')'
				cExpectMatch( TK_RPAREN, "behind the data type" )
			else
				a = hExpression( cprecedence(ASTCLASS_SIZEOF) )
			end if
			a = astNew( ASTCLASS_SIZEOF, a )

		'' DEFINED ['('] Identifier [')']
		case KW_DEFINED
			c.x += 1

			'' '('
			var have_parens = cMatch( TK_LPAREN )

			'' Identifier
			dim as string id
			if( tkGet( c.x ) = TK_ID ) then
				id = *tkSpellId( c.x )
			else
				cError( "expected identifier" + tkButFound( c.x ) )
				id = "<error-recovery>"
			end if
			a = cDefineLookup( id )
			if( a = NULL ) then
				a = astNewTEXT( id )
				cAddSymbol( a )
			end if
			a = astNew( ASTCLASS_CDEFINED, astNewSYM( a ) )
			c.x += 1

			if( have_parens ) then
				'' ')'
				cExpectMatch( TK_RPAREN, "to finish defined(...) expression" )
			end if

		case else
			cError( "expected expression" + tkButFound( c.x ) )
			a = astNew( ASTCLASS_CONSTI, "0" )
			astSetType( a, TYPE_INTEGER, NULL )
		end select
	end if

	'' Infix operators
	while( c.parseok )
		select case as const( tkGet( c.x ) )
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
		case TK_LBRACKET : op = ASTCLASS_INDEX   '' [ (a[b])
		case TK_DOT      : op = ASTCLASS_MEMBER  '' .
		case TK_ARROW    : op = ASTCLASS_MEMBERDEREF '' ->
		case else        : exit while
		end select

		'' Higher/same level means process now (takes precedence),
		'' lower level means we're done and the parent call will
		'' continue. The first call will start with level 0.
		var oplevel = cprecedence(op)
		if( oplevel < level ) then
			exit while
		end if
		'' Left associative?
		if( op <> ASTCLASS_IIF ) then
			oplevel += 1
		end if

		'' operator
		c.x += 1

		'' rhs
		var b = hExpression( oplevel )

		'' Handle ?: special case
		if( op = ASTCLASS_IIF ) then
			'' ':'
			cExpectMatch( TK_COLON, "for a?b:c iif operator" )

			a = astNewIIF( a, b, hExpression( oplevel ) )
		else
			'' Handle [] special case
			if( op = ASTCLASS_INDEX ) then
				'' ']'
				cExpectMatch( TK_RBRACKET, "for [] indexing operator" )
			end if

			a = astNew( op, a, b )
		end if
	wend

	function = a
end function

private function cExpression( ) as ASTNODE ptr
	function = hExpression( 0 )
end function

'' Initializer:
'' '{' ExpressionOrInitializer (',' ExpressionOrInitializer)* [','] '}'
private function cInitializer( ) as ASTNODE ptr
	'' '{'
	assert( tkGet( c.x ) = TK_LBRACE )
	c.x += 1

	var a = astNew( ASTCLASS_STRUCTINIT )

	do
		'' '}'?
		if( tkGet( c.x ) = TK_RBRACE ) then exit do

		astAppend( a, cExpressionOrInitializer( ) )

		'' ','
	loop while( cMatch( TK_COMMA ) and c.parseok )

	cExpectMatch( TK_RBRACE, "to close initializer" )

	function = a
end function

private function cExpressionOrInitializer( ) as ASTNODE ptr
	'' '{'?
	if( tkGet( c.x ) = TK_LBRACE ) then
		function = cInitializer( )
	else
		function = cExpression( )
	end if
end function

''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''

private sub cSkipToRparen( )
	do
		select case( tkGet( c.x ) )
		case TK_LPAREN, TK_LBRACKET, TK_LBRACE
			c.x = hFindClosingParen( c.x, (c.parentdefine <> NULL), TRUE )
		case TK_RPAREN, TK_EOF
			exit do
		end select
		c.x += 1
	loop
end sub

private sub cGccAttribute( byref gccattribs as integer )
	if( tkGet( c.x ) < TK_ID ) then
		cError( "expected attribute identifier inside __attribute__((...))" )
		exit sub
	end if

	var attr = *tkSpellId( c.x )

	'' Each attribute can be given as foo or __foo__ -- normalize to foo.
	if( (left( attr, 2 ) = "__") and (right( attr, 2 ) = "__") ) then
		attr = mid( attr, 3, len( attr ) - 4 )
	end if

	'' Most attributes aren't interesting for FB bindings and should be ignored,
	'' the main exception being the x86 calling conventions.
	select case( attr )
	case "alloc_size", _
	     "aligned", _
	     "always_inline", _
	     "const", _
	     "deprecated", _
	     "format", _
	     "format_arg", _
	     "gnu_inline", _
	     "malloc", _
	     "may_alias", _
	     "no_instrument_function", _
	     "noreturn", _
	     "pure", _
	     "sentinel", _
	     "unused", _
	     "visibility", _
	     "warn_unused_result"
		c.x += 1

		'' Some of these attributes accept further arguments which we
		'' can just ignore.
		cSkipToRparen( )

	case "cdecl"     : gccattribs or= ASTATTRIB_CDECL     : c.x += 1
	case "stdcall"   : gccattribs or= ASTATTRIB_STDCALL   : c.x += 1
	case "packed"    : gccattribs or= ASTATTRIB_PACKED    : c.x += 1
	case "dllimport" : gccattribs or= ASTATTRIB_DLLIMPORT : c.x += 1
	case else
		cError( "unknown attribute '" + *tkSpellId( c.x ) + "'" )
	end select
end sub

private sub cGccAttributeList( byref gccattribs as integer )
	while( c.parseok )
		select case( tkGet( c.x ) )
		case KW_VOLATILE, KW_INLINE, KW___INLINE, KW___INLINE__
			c.x += 1

		'' __attribute__((...)):
		'' __ATTRIBUTE__ '((' Attribute (',' Attribute)* '))'
		case KW___ATTRIBUTE__
			c.x += 1

			'' '('?
			cExpectMatch( TK_LPAREN, "as 1st '(' in '__attribute__((...))'" )

			'' '('?
			cExpectMatch( TK_LPAREN, "as 2nd '(' in '__attribute__((...))'" )

			'' Attribute (',' Attribute)*
			do
				'' ')'?
				if( tkGet( c.x ) = TK_RPAREN ) then exit do

				'' Attribute
				cGccAttribute( gccattribs )

				'' ','?
			loop while( cMatch( TK_COMMA ) and c.parseok )

			'' ')'?
			cExpectMatch( TK_RPAREN, "as 1st ')' in '__attribute__((...))'" )

			'' ')'?
			cExpectMatch( TK_RPAREN, "as 2nd ')' in '__attribute__((...))'" )

		case else
			exit while
		end select
	wend
end sub

''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''

'' Enum constant: Identifier ['=' Expression] (',' | '}')
private sub cEnumConst( )
	'' Identifier
	if( tkGet( c.x ) <> TK_ID ) then
		cError( "expected identifier for an enum constant" + tkButFound( c.x ) )
		exit sub
	end if
	var n = astNew( ASTCLASS_ENUMCONST, tkSpellId( c.x ) )
	dim existing as ASTNODE ptr
	if( frog.syntaxonly = FALSE ) then
		existing = cLocalNameLookup( n->text )
		if( existing ) then
			cOops( astDumpPrettyDecl( n ) + " redefined" )
		end if
	end if
	c.x += 1

	'' '='?
	if( cMatch( TK_EQ ) ) then
		'' Expression
		n->expr = cExpression( )
	end if

	'' (',' | '}')
	select case( tkGet( c.x ) )
	case TK_COMMA
		c.x += 1

	case TK_RBRACE

	case else
		cError( "expected ',' or '}' behind enum constant" + tkButFound( c.x ) )
	end select

	if( c.parseok ) then
		if( existing = NULL ) then
			cAddSymbol( n )
		end if
		cAppendNode( n )
	end if
end sub

'' Decide whether an anonymous struct/union/enum body is "standalone",
'' or "inline" as part of a declaration.
private function hIsStandaloneTagBody( byval y as integer, byref followingid as string ) as integer
	'' '{'
	assert( tkGet( y ) = TK_LBRACE )

	'' '}'?
	y = hFindClosingParen( y, (c.parentdefine <> NULL), TRUE )
	if( tkGet( y ) <> TK_RBRACE ) then
		exit function
	end if
	y += 1

	'' __attribute__(...)?
	if( (tkGet( y ) = KW___ATTRIBUTE__) and (tkGet( y + 1 ) = TK_LPAREN) ) then
		y += 1
		y = hFindClosingParen( y, (c.parentdefine <> NULL), TRUE )
	end if

	'' ';'? If there is a ';' directly behind the struct body, then it's
	'' not a declaration, but just the struct. If there's a declarator,
	'' then it's a declaration.
	select case( tkGet( y ) )
	case TK_SEMI
		return TRUE
	case TK_ID
		followingid = *tkSpellId( y )
	end select

	function = FALSE
end function

'' Generate an id for an unnamed tag declared "inline" as part of another declaration
'' For example:
''    struct A {
''        struct {
''            int i;
''        } x;
''    };
'' becomes:
''    type __A_x
''        i as long
''    end type
''    type A
''        x as __A_x
''    end type
private function hGenerateTagId( byref followingid as string ) as string
	var id = "_"

	'' Make it context-specific (this way we can hopefully avoid conflicts
	'' between different bindings, and still get nice merging behaviour),
	'' by prefixing all the parent "namespaces"
	for i as integer = 1 to c.namespacelevel
		var owner = c.namespaces(i).owner
		if( (owner->text <> NULL) and ((owner->attrib and ASTATTRIB_GENERATEDID) = 0) ) then
			id += "_" + *owner->text
		end if
	next

	'' Also include the name (if known) of the field/var/proc/... whose
	'' declaration contained this tag definition.
	if( len( followingid ) > 0 ) then
		id += "_" + followingid
	else
		'' Use a context-specific counter
		with( c.namespaces(c.namespacelevel) )
			id += "_" & .dummyidcounter
			.dummyidcounter += 1
		end with
	end if

	function = id
end function

'' {STRUCT|UNION|ENUM} [Identifier] '{' StructBody|EnumBody '}'
'' {STRUCT|UNION|ENUM} Identifier
private function cTag( ) as ASTNODE ptr
	'' {STRUCT|UNION|ENUM}
	dim as integer astclass
	select case( tkGet( c.x ) )
	case KW_UNION
		astclass = ASTCLASS_UNION
	case KW_ENUM
		astclass = ASTCLASS_ENUM
	case else
		assert( tkGet( c.x ) = KW_STRUCT )
		astclass = ASTCLASS_STRUCT
	end select
	c.x += 1

	'' __attribute__((...))
	dim gccattrib as integer
	cGccAttributeList( gccattrib )

	'' [Identifier]
	dim tagid as zstring ptr
	if( tkGet( c.x ) = TK_ID ) then
		tagid = tkSpellId( c.x )
		c.x += 1
	end if

	'' '{'?
	var is_body = (tkGet( c.x ) = TK_LBRACE)

	'' Non-body tag references can't be anonymous
	if( (not is_body) and (tagid = NULL) ) then
		cError( "expected '{' or tag name" + tkButFound( c.x ) )
		tagid = @"<error-recovery>"
	end if

	''
	'' Named tag bodies go to the current scope, potentially overriding bodies from parent scopes.
	'' Duplicates in the same scope aren't allowed.
	''
	'' Non-body tags on the other hand reference the "nearest" tag body, from the same scope,
	'' or from a parent scope. If there is no body, all references with the same name point to
	'' the same toplevel node (until a body is defined; from then on all further references
	'' within that scope will reference the body there).
	''
	dim tag as ASTNODE ptr
	if( tagid ) then
		if( is_body ) then
			if( frog.syntaxonly = FALSE ) then
				tag = cLocalTagLookup( tagid )
			end if
		else
			tag = cFullTagLookup( tagid )
		end if
		if( tag ) then
			if( (tag->class <> astclass) and (not frog.syntaxonly) ) then
				cOops( astDumpPrettyDecl( tag ) + " redeclared as different kind of tag" )
			end if
		end if
	end if

	if( tag = NULL ) then
		tag = astNew( astclass, tagid )
		if( is_body ) then
			'' Add new non-anonymous tags to current scope
			if( tag->text ) then
				cAddSymbol( tag )
			end if
		else
			if( c.filterout ) then
				'' Filtering out the first use of a tag, so don't add
				'' the forward reference (if any) here. A forward reference
				'' should be added in the file containing the first use.
				tag->attrib or= ASTATTRIB_DONTADDFWDREF
			end if

			'' Add new non-body tags to toplevel scope
			cAddToplevelSymbol( tag )

			if( tag->text ) then
				cAddTag( tag )
			end if
		end if
	end if

	tag->attrib or= gccattrib

	if( is_body ) then
		if( tag->attrib and ASTATTRIB_BODYDEFINED ) then
			cOops( astDumpPrettyDecl( tag ) + " body redefined" )
		end if
		tag->attrib or= ASTATTRIB_BODYDEFINED

		if( c.filterout ) then
			'' If filtering out a body, the filtered-out code will
			'' be #included at the top (i.e. the code will be re-ordered),
			'' so no forward reference is needed anymore...
			tag->attrib or= ASTATTRIB_DONTADDFWDREF
		end if

		select case( astclass )
		case ASTCLASS_STRUCT, ASTCLASS_UNION
			var maxalign = c.pragmapack.stack(c.pragmapack.level)

			'' Preserve alignment if needed so we can emit FIELD = N,
			'' but not if N >= 8, because FB has no alignment > 8,
			'' so FIELD >= 8 is useless. Omitting it improves merging
			'' for some bindings.
			if( (maxalign > 0) and (maxalign < 8) ) then
				tag->maxalign = maxalign
			end if
		end select

		var is_nested_struct = FALSE

		'' Anonymous?
		if( tag->text = NULL ) then
			dim followingid as string
			if( hIsStandaloneTagBody( c.x, followingid ) ) then
				'' Anonymous standalone tag body
				'' struct/union:
				''    it's an anonymous nested struct/union (assuming we're inside another struct/union)
				''    which can be anonymous & nested in FB too.
				'' enum:
				''    it's just an anonymous enum
				is_nested_struct = (astclass <> ASTCLASS_ENUM)
			else
				'' Anonymous "inline" tag body (part of a declaration)
				'' We must give it an a id, because FB doesn't have "inline" UDTs/enums.
				'' (it's not an anonymous nested struct/union or anonymous enum)
				astSetText( tag, hGenerateTagId( followingid ) )
				tag->attrib or= ASTATTRIB_GENERATEDID
			end if
		end if

		'' '{'
		c.x += 1

		if( c.parseok and (not c.filterout) ) then
			if( is_nested_struct ) then
				'' anonymous nested struct/union:
				'' Add to current block as-is, inside the parent struct/union.
				astAppend( c.blocks(c.blocklevel).context, tag )
			else
				'' enum, normal struct/union:
				'' Add to the current scope, as opposed to the current block.
				'' FB doesn't allow UDTs to be declared inside others.
				'' Inside another struct?
				var scopeowner = c.scopes(c.scopelevel).owner
				var namespaceowner = c.namespaces(c.namespacelevel).owner
				if( namespaceowner <> scopeowner ) then
					'' The parent struct must already be added to the current scope.
					'' All new structs from inside a struct must be added in front of it,
					'' because it could reference them.
					var i = scopeowner->tail
					while( i <> namespaceowner )
						i = i->prev
					wend
					astInsert( scopeowner, tag, i )
				else
					astAppend( scopeowner, tag )
				end if
			end if
		end if

		'' normal struct/union:
		''    Open new namespace for capturing the fields.
		'' enum, anonymous nested struct/union:
		''    No new namespace; symbols are added to the parent's namespace
		'' No new scope in either case, because we don't want to capture tags
		'' inside this tag's body.
		cPush( tag, (astclass <> ASTCLASS_ENUM) and (not is_nested_struct), FALSE )

		'' Parse struct/union/enum body
		cBody( )

		cPop( )

		'' '}'
		cExpectMatch( TK_RBRACE, "to close " + astDumpPrettyDecl( tag ) + " block" )

		'' __attribute__((...))
		cGccAttributeList( tag->attrib )
	end if

	function = tag
end function

private sub cTypedef( )
	'' TYPEDEF
	c.x += 1
	cDeclaration( ASTCLASS_TYPEDEF, 0 )
end sub

''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''

''
'' Determine whether a sequence of tokens starting with '{' is a scope block or
'' an array/struct initializer.
''
'' If there is a ';' behind the first "element" then it surely is a scope block.
'' If there's a ',' instead, then it probably is an initializer. An empty '{}'
'' is treated as initializer. We can't just stop at the first ',' though. In
'' order to support "{ int a, b; }", we have to scan the whole '{...}' block
'' for ';'s.
''
private function hDefineBodyLooksLikeScopeBlock( byval x as integer ) as integer
	'' '{'
	assert( tkGet( x ) = TK_LBRACE )

	do
		x += 1

		select case( tkGet( x ) )
		case TK_SEMI
			return TRUE

		case TK_EOL, TK_RBRACE
			exit do

		case TK_LPAREN, TK_LBRACKET, TK_LBRACE
			x = hFindClosingParen( x, TRUE, TRUE )
		end select
	loop

	function = FALSE
end function

'' Return value: whether to keep the #define
private function cDefineBody( byval macro as ASTNODE ptr ) as integer
	select case( tkGet( c.x ) )
	'' Don't preserve #define if it just contains _Pragma's
	'' _Pragma("...")
	case KW__PRAGMA
		do
			'' _Pragma
			c.x += 1

			'' '('
			if( tkGet( c.x ) <> TK_LPAREN ) then exit do
			c.x += 1

			'' Skip to ')' - we don't care whether there is
			'' a string literal or something like a #macroparam or similar...
			cSkipToRparen( )
			c.x += 1
		loop while( tkGet( c.x ) = KW__PRAGMA )

		exit function

	case KW___ATTRIBUTE__
		'' Don't preserve #define if it just contains an __attribute__
		cGccAttributeList( 0 )
		exit function

	'' '{'
	case TK_LBRACE
		if( hDefineBodyLooksLikeScopeBlock( c.x ) ) then
			macro->expr = cScope( NULL )
		else
			macro->expr = cInitializer( )
		end if
		return TRUE

	'' Just a 'const'? It's common to have a #define for the const keyword
	'' in C headers...
	case KW_CONST
		if( tkGet( c.x + 1 ) = TK_EOL ) then
			'' const
			c.x += 1
			exit function
		end if
	end select

	if( hIsDataTypeOrAttribute( c.x ) ) then
		macro->expr = cDataType( )
		return TRUE
	end if

	macro->expr = cExpression( )
	function = TRUE
end function

private function hDefBodyContainsIds( byval y as integer ) as integer
	do
		select case( tkGet( y ) )
		case TK_EOL
			exit do
		case TK_ID
			return TRUE
		end select
		y += 1
	loop
end function

private sub cParseDefBody( byval n as ASTNODE ptr, byref add_to_ast as integer )
	c.parentdefine = n
	cPush( n, TRUE, TRUE )

	'' Body
	var bodybegin = c.x
	add_to_ast and= cDefineBody( n )

	'' Didn't reach EOL? Then the beginning of the macro body could
	'' be parsed as expression, but not the rest.
	if( tkGet( c.x ) <> TK_EOL ) then
		cError( "failed to parse full #define body" )
		c.x = hSkipToEol( c.x )
	end if

	'' Turn the #define's body into an UNKNOWN if parsing failed
	if( c.parseok = FALSE ) then
		n->expr = astNewUNKNOWN( bodybegin, c.x )
		c.parseok = TRUE
	end if

	cPop( )
	c.parentdefine = NULL
end sub

private sub cDefine( )
	assert( c.blocklevel = 0 )

	var begin = c.x - 1
	c.x += 1

	'' Identifier ['(' ParameterList ')']
	var macro = hDefineHead( c.x )

	var existing = cDefineLookup( macro->text )
	if( existing ) then
		'' There can already be a TEXT node (undeclared id) in case it was used in a #define body
		'' before being declared. If so, we want to turn the TEXT into a proper declaration.
		assert( existing->class = ASTCLASS_TEXT )
		astCopy( existing, macro )
		macro = existing
	end if

	'' Add before trying to parse the body, because if that will be delayed,
	'' then it would be added before the body is parsed anyways.
	cAddSymbol( macro )

	var add_to_ast = not c.filterout

	'' Body?
	assert( macro->expr = NULL )
	if( tkGet( c.x ) <> TK_EOL ) then
		if( hDefBodyContainsIds( c.x ) ) then
			'' Delay parsing, until we've parsed all declarations in the input.
			'' This way we have more knowledge about typedefs etc. which could
			'' help parsing this #define body.
			cAddDefBody( begin, c.x, macro )
			c.x = hSkipToEol( c.x )
		else
			'' Probably a simple #define body, parse right now
			cParseDefBody( macro, add_to_ast )
		end if
	end if

	'' Eol
	assert( tkGet( c.x ) = TK_EOL )
	c.x += 1

	if( add_to_ast ) then
		cAppendNode( macro )
	end if
end sub

private function cPragmaPackNumber( ) as integer
	var n = cNumberLiteral( )
	if( n->class <> ASTCLASS_CONSTI ) then
		exit function
	end if
	c.pragmapack.stack(c.pragmapack.level) = astEvalConstiAsInt64( n )
	function = TRUE
end function

private function cPragmaPack( ) as integer
	'' pack
	assert( tkGet( c.x ) = TK_ID )
	assert( tkSpell( c.x ) = "pack" )
	c.x += 1

	'' '('
	cExpectMatch( TK_LPAREN, "as in '#pragma pack(...)'" )

	select case( tkGet( c.x ) )
	'' #pragma pack(N): Set max alignment for current top of stack
	case TK_NUMBER
		if( cPragmaPackNumber( ) = FALSE ) then
			exit function
		end if

	'' #pragma pack(push, N)
	'' #pragma pack(pop)
	case TK_ID
		select case( *tkSpellId( c.x ) )
		case "push"
			c.pragmapack.level += 1
			if( c.pragmapack.level >= c.pragmapack.MAXLEVEL ) then
				oops( "#pragma pack stack too small" )
			end if
			cResetPragmaPack( )
			c.x += 1

			'' ','
			cExpectMatch( TK_COMMA, "behind 'push'" )

			'' 'N'
			if( tkGet( c.x ) <> TK_NUMBER ) then
				exit function
			end if
			if( cPragmaPackNumber( ) = FALSE ) then
				exit function
			end if

		case "pop"
			if( c.pragmapack.level > 0 ) then
				c.pragmapack.level -= 1
			else
				cError( "#pragma pack(pop) without previous push" )
			end if
			c.x += 1

		case else
			exit function
		end select

	'' #pragma pack(): Reset top of stack to default
	case TK_RPAREN
		cResetPragmaPack( )

	case else
		exit function
	end select

	'' ')'
	cExpectMatch( TK_RPAREN, "as in '#pragma pack(...)'" )

	'' Eol
	assert( tkGet( c.x ) = TK_EOL )
	c.x += 1

	function = TRUE
end function

'' #pragma comment(lib, "...")
private sub cPragmaComment( )
	'' comment
	assert( tkGet( c.x ) = TK_ID )
	assert( tkSpell( c.x ) = "comment" )
	c.x += 1

	'' '('
	assert( tkGet( c.x ) = TK_LPAREN )
	c.x += 1

	'' lib
	assert( tkGet( c.x ) = TK_ID )
	assert( tkSpell( c.x ) = "lib" )
	c.x += 1

	'' ','
	assert( tkGet( c.x ) = TK_COMMA )
	c.x += 1

	'' "<library-file-name>"
	assert( tkGet( c.x ) = TK_STRING )
	var libname = *tkGetText( c.x )
	c.x += 1

	'' ')'
	assert( tkGet( c.x ) = TK_RPAREN )
	c.x += 1

	assert( tkGet( c.x ) = TK_EOL )
	c.x += 1

	''
	'' Turn the #pragma comment(lib, "...") into #inclib "..."
	''
	'' It seems to be common to specify the library's full file name in the
	'' #pragma directive, i.e. "foo.lib". In FB it must be #inclib "foo"
	'' though, no extension or lib prefix. Thus, we need to do some
	'' conversion.
	''
	'' Besides "foo.lib", we also handle "libfoo.a" here which is another
	'' common library file name format. Anything else should probably be
	'' passed through as-is though.
	''

	'' Remove .lib suffix
	if( right( libname, 4 ) = ".lib" ) then
		libname = left( libname, len( libname ) - 4 )
	'' Remove lib prefix and .a suffix
	elseif( (left( libname, 3 ) = "lib") and (right( libname, 2 ) = ".a") ) then
		libname = right( libname, len( libname ) - 3 )
		libname = left( libname, len( libname ) - 2 )
	end if

	if( c.filterout = FALSE ) then
		cAppendNode( astNew( ASTCLASS_INCLIB, libname ) )
	end if
end sub

''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''

''
'' Declaration base type parsing
''
'' The base type is the data type part of a variable/procedure/typedef/parameter
'' declaration that is at the front, in front of the identifier list.
'' '*' chars indicating pointers belong to the identifier, not the type.
''
''    int a, b, c;
''    ^^^
''
''    struct UDT const *p, **pp;
''    ^^^^^^^^^^^^^^^^
''
''    struct { ...fields... } a;
''    ^^^^^^^^^^^^^^^^^^^^^^^
''
'' Besides the base type there can be modifiers such as "signed", "unsigned",
'' "const", "short", "long". They can be used together with some base types,
'' for example "short int a;", or alone: "short a;". Modifiers can appear in
'' front of the base type or behind it, in any order. Some modifiers are
'' incompatible to each-other, such as "signed" and "unsigned", or "short" and
'' "long". There may only be 1 "short", and only 1 or 2 "long"s.
''
''    short int a;
''    unsigned a;
''    const int unsigned a;
''    long const a;
''    long long int a;
''    const const unsigned long const long const int const unsigned a;
''
private sub cBaseType _
	( _
		byref dtype as integer, _
		byref subtype as ASTNODE ptr, _
		byref gccattribs as integer, _
		byval astclass as integer _
	)

	dtype = TYPE_NONE
	subtype = NULL

	var signedmods = 0
	var unsignedmods = 0
	var constmods = 0
	var shortmods = 0
	var longmods = 0

	''
	'' 1. Parse base type and all modifiers, and count them
	''

	while( c.parseok )
		'' __ATTRIBUTE__((...))
		cGccAttributeList( gccattribs )

		select case( tkGet( c.x ) )
		case KW_SIGNED
			if( unsignedmods > 0 ) then
				cError( "mixed SIGNED with previous UNSIGNED modifier" )
			end if
			signedmods += 1

		case KW_UNSIGNED
			if( signedmods > 0 ) then
				cError( "mixed UNSIGNED with previous SIGNED modifier" )
			end if
			unsignedmods += 1

		case KW_CONST
			constmods += 1

		case KW_SHORT
			if( longmods > 0 ) then
				cError( "mixed SHORT with previous LONG modifier" )
			end if
			shortmods += 1
			if( shortmods > 1 ) then
				cError( "more than 1 SHORT modifier" )
			end if

		case KW_LONG
			if( shortmods > 0 ) then
				cError( "mixed LONG with previous SHORT modifier" )
			end if
			longmods += 1
			if( longmods > 2 ) then
				cError( "more than 2 LONG modifiers" )
			end if

		case else
			'' Only one base type is allowed
			if( dtype <> TYPE_NONE ) then
				exit while
			end if

			select case( tkGet( c.x ) )
			case KW_ENUM, KW_STRUCT, KW_UNION
				dtype = TYPE_UDT
				subtype = cTag( )
				c.x -= 1

			case TK_ID
				''
				'' An identifier can be part of the data type if
				'' it's a typedef (the code here doesn't check
				'' for that but just assumes it is).
				''
				'' Modifiers such as CONST can be combined with
				'' such typedefs, others like UNSIGNED can't.
				'' For example:
				''
				''    typedef int myint;
				''
				''    const myint;      // doesn't declare anything
				''    const myint foo;  // CONST combined with myint typedef
				''
				''    unsigned foo;       // ok, foo = unsigned int variable
				''    unsigned myint foo; // invalid code, myint = variable name, foo = unexpected token
				''

				'' Already saw modifiers that themselves are enough to form the type?
				if( signedmods or unsignedmods or longmods or shortmods ) then
					'' Then don't treat this id as the type
					exit while
				end if

				'' Treat the id as the type
				var id = tkSpellId( c.x )
				dtype = extradatatypesLookup( id )
				if( dtype = TYPE_NONE ) then
					dtype = TYPE_UDT
					subtype = cFullNameLookup( id )
					if( subtype ) then
						select case( subtype->class )
						case ASTCLASS_TYPEDEF, ASTCLASS_TEXT
							'' It can be an existing typedef, or an unknown id
						case else
							cOops( "expected data type" + tkButFound( c.x ) + ", which I think refers to " + astDumpPrettyDecl( subtype ) + ". If that should be a data type, then some input is missing (or some macros weren't expanded)." )
						end select
						subtype = hSolveTypedefOutIfWanted( subtype )
					else
						'' Unknown identifier; assume it's a typedef
						subtype = astNewTEXT( id )
						cAddSymbol( subtype )
					end if
				end if

			case KW_VOID   : dtype = TYPE_ANY
			case KW_FLOAT  : dtype = TYPE_SINGLE
			case KW_DOUBLE : dtype = TYPE_DOUBLE
			case KW_CHAR   : dtype = TYPE_ZSTRING
			case KW_INT    : dtype = TYPE_LONG
			case KW__BOOL  : dtype = TYPE_BYTE

			case else
				exit while
			end select
		end select

		c.x += 1
	wend

	'' Some details can only be decided after parsing the whole thing,
	'' because for example "unsigned int" and "int unsigned" both are allowed.
	select case( dtype )
	case TYPE_DOUBLE
		if( longmods = 1 ) then
			dtype = TYPE_CLONGDOUBLE
			if( c.filterout = FALSE ) then
				api->uses_clongdouble = TRUE
			end if
		end if

	case TYPE_ZSTRING
		'' SIGNED|UNSIGNED CHAR becomes BYTE|UBYTE,
		'' but plain CHAR probably means ZSTRING
		if( signedmods > 0 ) then
			dtype = TYPE_BYTE
		elseif( unsignedmods > 0 ) then
			dtype = TYPE_UBYTE
		end if

	case TYPE_LONG, TYPE_NONE
		'' Base type is "int" (either explicitly given, or implied
		'' because no other base type was given). Any modifiers are
		'' just added on top of that.
		if( shortmods = 1 ) then
			dtype = iif( unsignedmods > 0, TYPE_USHORT, TYPE_SHORT )
		elseif( longmods = 1 ) then
			if( frog.clong32 ) then
				'' C long => LONG (ok on Windows where C long is always 32bit)
				dtype = iif( unsignedmods > 0, TYPE_ULONG, TYPE_LONG )
			else
				'' C long => CLONG
				dtype = iif( unsignedmods > 0, TYPE_CULONG, TYPE_CLONG )
				if( c.filterout = FALSE ) then
					api->uses_clong = TRUE
				end if
			end if
		elseif( longmods = 2 ) then
			dtype = iif( unsignedmods > 0, TYPE_ULONGINT, TYPE_LONGINT )
		elseif( dtype = TYPE_LONG ) then
			'' Explicit "int" base type and no modifiers
			dtype = iif( unsignedmods > 0, TYPE_ULONG, TYPE_LONG )
		elseif( unsignedmods > 0 ) then
			'' UNSIGNED only
			dtype = TYPE_ULONG
		elseif( signedmods > 0 ) then
			'' SIGNED only
			dtype = TYPE_LONG
		else
			'' No modifiers and no explicit "int" either
			cError( "expected a data type" + tkButFound( c.x ) )
		end if
	end select

	select case( dtype )
	case TYPE_ANY, TYPE_SINGLE, TYPE_DOUBLE, TYPE_UDT
		if( signedmods or unsignedmods or shortmods or longmods ) then
			cError( "SIGNED|UNSIGNED|SHORT|LONG modifiers used with void/float/double/typedef/UDT" )
		end if
	case TYPE_ZSTRING, TYPE_BYTE, TYPE_UBYTE
		if( shortmods or longmods ) then
			cError( "SHORT|LONG modifiers used with CHAR type" )
		end if
	end select

	'' Any CONSTs on the base type are merged into one
	''    const int a;
	''    const int const a;
	''          int const a;
	''    const const int const const a;
	'' It's all the same...
	if( constmods > 0 ) then
		dtype = typeSetIsConst( dtype )
	end if

	'' __ATTRIBUTE__((...))
	cGccAttributeList( gccattribs )
end sub

'' ParamDeclList = ParamDecl (',' ParamDecl)*
'' ParamDecl = '...' | Declaration{Param}
private sub cParamDeclList( )
	do
		'' '...'?
		if( tkGet( c.x ) = TK_ELLIPSIS ) then
			cAppendNode( astNew( ASTCLASS_PARAM ) )
			c.x += 1
		else
			cDeclaration( ASTCLASS_PARAM, 0 )
		end if

		'' ','?
	loop while( cMatch( TK_COMMA ) and c.parseok )
end sub

private function hCanHaveInitializer( byval n as ASTNODE ptr ) as integer
	select case( n->class )
	case ASTCLASS_PARAM
		function = TRUE
	case ASTCLASS_VAR
		function = ((n->attrib and ASTATTRIB_EXTERN) = 0)
	end select
end function

private sub hTypedefExpansionFailed( byval n as ASTNODE ptr )
	cError( "can't solve out " + astDumpPrettyDecl( n->subtype ) + " in " + astDumpPrettyDecl( n ) )
end sub

private sub hExpandArrayTypedef( byval n as ASTNODE ptr )
	'' Array types can only be solved out if it's not a pointer (FB doesn't support pointers to arrays)
	if( typeGetPtrCount( n->dtype ) > 0 ) then
		hTypedefExpansionFailed( n )
		exit sub
	end if

	var typedef = n->subtype
	astSetType( n, typeGetConst( n->dtype ) or typedef->dtype, typedef->subtype )
	if( n->array = NULL ) then
		n->array = astNew( ASTCLASS_ARRAY )
	end if
	astAppend( n->array, astCloneChildren( typedef->array ) )
end sub

''
'' Expand the function typedef used in the given declaration.
''
'' For example, given a declaration using a function typedef like this one:
''    typedef int (F)(int);
''
'' we want to expand the typedef into the declaration:
''    F f;             =>    int f1(int);            (what looked like a var really is a proc)
''    F *p;            =>    int (*p)(int);          (procptr var)
''    void f(F *p);    =>    void f(int (*p)(int));  (procptr param)
''    typedef F G;     =>    typedef int (G)(int);   (and then later we process G too)
''
'' because FB doesn't support function typedefs.
''
'' If there's a CONST on the function type, e.g.:
''    typedef int (T)(int);
''    T const f;
'' it will be overwritten and lost, i.e. we ignore it. According to
'' gcc -pedantic, "ISO C forbids qualified function types", and it seems like
'' the C++ standard requires cv-qualifiers to be ignored in such cases.
''
'' When expanding into a pointer, some of the existing CONSTs there need to be
'' preserved though:
''    a * const p1;
'' becomes:
''    int (* const p1)(int);
'' But as above, cv-qualifiers on the function type don't make sense and should
'' be ignored, e.g. in case of:
''    a const * p1;
''
private sub hExpandProcTypedef( byval n as ASTNODE ptr )
	var typedef = n->subtype

	select case( n->class )
	case ASTCLASS_VAR, ASTCLASS_FIELD
		'' Not a pointer?
		if( typeGetPtrCount( n->dtype ) = 0 ) then
			'' This is ok for vars/fields; they just become procedures themselves.
			'' (although with fields that can only really happen in C++ code)
			var proc = typedef->subtype
			assert( proc->class = ASTCLASS_PROC )

			'' Copy over the function result type (overwriting any previous cv-qualifiers),
			'' parameters, and callconv attributes
			n->class = ASTCLASS_PROC
			n->dtype = proc->dtype
			n->subtype = proc->subtype
			assert( n->head = NULL )
			astAppend( n, astCloneChildren( proc ) )
			n->attrib or= proc->attrib and (not ASTATTRIB_FILTEROUT)

			exit sub
		end if

		'' var/field; it's a pointer to the function type; ok

	case ASTCLASS_TYPEDEF, ASTCLASS_PARAM
		'' Expanding typedef in another typedef; always ok
		'' Expanding into param, also ok (it becomes a function pointer;
		'' handled later in hPostprocessDeclarator())

	case else
		'' Anywhere else (params, function results, casts, sizeof):
		'' must be a pointer to the function type, or we can't expand the typedef
		if( typeGetPtrCount( n->dtype ) = 0 ) then
			hTypedefExpansionFailed( n )
			exit sub
		end if
	end select

	'' Insert the typedef's type, overwriting the use of the typedef
	assert( typeGetDtAndPtr( typedef->dtype ) = TYPE_PROC )
	n->dtype = typeUnsetBaseConst( typeSetDt( n->dtype, TYPE_PROC ) )
	n->subtype = typedef->subtype
	'' The PROC subtype must be duplicated if inside a #define body so it
	'' can be given unique callconv treatment by astHideCallConv().
	if( c.parentdefine ) then
		n->subtype = astClone( n->subtype )
	end if
end sub

private sub hPostprocessDeclarator( byval n as ASTNODE ptr )
	if( typeGetDt( n->dtype ) = TYPE_UDT ) then
		select case( n->subtype->class )
		case ASTCLASS_TYPEDEF
			''
			'' Solve out array & function typedefs
			''
			'' Array/function typedefs are not supported in FB. If this declaration
			'' uses such a type as its data type, we can solve out the typedef and
			'' turn the declaration itself into an array or procedure, respectively.
			''
			'' Since typedefs are also processed this way here, we can be sure that
			'' typedefs themselves are already fully expanded when reaching another
			'' declaration using one of these typedefs. Thus, expanding a typedef
			'' doesn't cause a need for further expansions.
			''
			if( n->subtype->array ) then
				hExpandArrayTypedef( n )
			else
				var typedefdtype = n->subtype->dtype
				select case( typeGetDtAndPtr( typedefdtype ) )
				case TYPE_PROC
					hExpandProcTypedef( n )
				case TYPE_ZSTRING, TYPE_WSTRING
					n->dtype = typeMultAddrOf( typedefdtype, typeGetPtrCount( n->dtype ) ) _
							or typeGetConst( n->dtype )
					n->subtype = NULL
				end select
			end if

		case ASTCLASS_STRUCT, ASTCLASS_UNION, ASTCLASS_ENUM
			var tag = n->subtype

			''
			'' Detect usage of declared but undefined tags
			''
			'' Tags may be used before being defined. FB does not support implicit forward
			'' references like that; instead, forward references can only be introduced by
			'' explicit typedefs. And even then, only the name used in the forward-declaration
			'' and not the forward-reference itself can be used (until the forward-reference
			'' is "resolved"), except in other typedefs (which can be forward declarations
			'' using the same forward-reference).
			''
			'' Thus, we may need to add a forward reference for a tag if it's used before being defined.
			'' However, we don't want to do that if such usage occurs only as part of typedefs, because
			'' then it's ok for FB. It's best to avoid unnecessary forward declarations.
			''
			'' Undefined?
			if( ((tag->attrib and ASTATTRIB_BODYDEFINED) = 0) and (n->class <> ASTCLASS_TYPEDEF) ) then
				tag->attrib or= ASTATTRIB_USEBEFOREDEF
			end if
		end select
	end if

	select case( n->class )
	case ASTCLASS_PARAM
		select case( typeGetDtAndPtr( n->dtype ) )
		'' Remap "byval as jmp_buf" to "byval as jmp_buf ptr"
		'' jmp_buf is defined as array type in C, and arrays are passed byref in C.
		'' Since FB's crt/setjmp.bi defines jmp_buf as a simple UDT, we have to handle
		'' the "byref" part manually. Strictly speaking fbfrog should produce "byref as jmp_buf",
		'' but traditionally FB's headers always used a "byval as jmp_buf ptr"...
		case TYPE_UDT
			if( *n->subtype->text = "jmp_buf" ) then
				n->dtype = typeAddrOf( n->dtype )
			end if

		'' Function type? It's really just a function pointer
		case TYPE_PROC
			n->dtype = typeAddrOf( n->dtype )

		end select

		'' C array parameter? It's really just a pointer (the array is passed byref).
		'' FB doesn't support C array parameters like that, so turn them into pointers:
		''    int a[5]  ->  byval a as long ptr
		if( n->array ) then
			n->array = NULL
			n->dtype = typeAddrOf( n->dtype )
		end if

	case ASTCLASS_PROC
		'' Ignore extern on procedures, not needed explicitly
		n->attrib and= not ASTATTRIB_EXTERN

		'' Default to cdecl, if there's no explicit callconv attribute yet
		if( (n->attrib and ASTATTRIB__CALLCONV) = 0 ) then
			n->attrib or= ASTATTRIB_CDECL

		'' And show an error if conflicting attributes were given
		'' (perhaps we just made a mistake assigning them - better safe...)
		elseif( (n->attrib and ASTATTRIB__CALLCONV) = ASTATTRIB__CALLCONV ) then
			cError( "cdecl/stdcall attributes specified together" )
		end if

		if( c.filterout = FALSE ) then
			if( n->attrib and ASTATTRIB_CDECL ) then
				api->cdecls += 1
			elseif( n->attrib and ASTATTRIB_STDCALL ) then
				api->stdcalls += 1
			end if
		end if
	end select

	''
	'' Handle declarations with plain char/zstring or wchar_t/wstring data type.
	''
	'' If it's a char pointer or array, then it's probably supposed to be a string,
	'' and we can leave it as-is (zstring/wstring). If it's just a char, then it's
	'' probably supposed to be a byte (or wchar_t) though.
	''
	'' FB doesn't allow "foo as zstring" - it must be a pointer or fixed-length string,
	'' except in typedefs. Typedefs are a special case - char/wchar_t means byte/wchar_t
	'' or zstring/wstring depending on where they're used. Because of this we expand
	'' these typedefs like array/proc typedefs. To allow this expansion to work,
	'' we keep the zstring/wstring type on the typedefs.
	''
	select case( typeGetDtAndPtr( n->dtype ) )
	case TYPE_ZSTRING, TYPE_WSTRING
		if( n->array ) then
			'' Use the last (inner-most) array dimension as the fixed-length string size
			var d = n->array->tail
			assert( d->class = ASTCLASS_DIMENSION )
			assert( d->expr )
			n->subtype = astClone( d->expr )
			astRemove( n->array, d )

			'' If no dimensions left, remove the array type entirely
			if( n->array->head = NULL ) then
				n->array = NULL
			end if
		elseif( n->class <> ASTCLASS_TYPEDEF ) then
			'' Turn zstring/wstring into byte/wchar_t, but preserve CONSTs
			if( typeGetDtAndPtr( n->dtype ) = TYPE_ZSTRING ) then
				n->dtype = typeGetConst( n->dtype ) or TYPE_BYTE
			else
				if( c.filterout = FALSE ) then
					api->uses_wchar_t = TRUE
				end if
				n->dtype = typeGetConst( n->dtype ) or TYPE_WCHAR_T
			end if
		end if
	end select

	'' Visit procptr subtypes
	if( typeGetDt( n->dtype ) = TYPE_PROC ) then
		hPostprocessDeclarator( n->subtype )
	end if
end sub

''
'' Declarator =
''    GccAttributeList
''    ('*' [CONST|GccAttributeList])*
''    { [Identifier] | '(' Declarator ')' }
''    { '(' ParamList ')' | ('[' ArrayElements ']')* }
''    GccAttributeList
''
'' This needs to parse things like:
''    i            for example, as part of: int i;
''    i[10]        array: int i[10];
''    <nothing>    anonymous parameter: int f(int);
''    ()           empty parameter list for anonymous parameter: int f(int ());
''    ***p         int ***p;
''    (*p)(void)   function pointer: void (*p)(void);
''    (((i)))      extra parentheses around identifier: int (((i)));
''    *(*(pp))     ditto
''    (*f(void))(void)    function returning a function pointer:
''                            void (*f(void))(void);
''    (*p[10])(void)      array of function pointers: void (*p[10])(void);
''
'' Example 1:
''
''         int (*f)(int a);
'' depth 1:     ^^
''    innerprocptrdtype = TYPE_PROC (unused)
''    procptrdtype      = typeAddrOf( TYPE_PROC )
''    innernode = NULL
''    node = NULL
''    t = VAR( f as int ptr )    (f as int ptr for now, in case it's just "int (*f);")
''
''         int (*f)(int a);
'' depth 0:    ^  ^^^^^^^^
''    innerprocptrdtype = typeAddrOf( TYPE_PROC )      (passed up from depth 1)
''    procptrdtype      = TYPE_PROC                    (unused)
''    innernode = VAR( f as int ptr )                  (passed up from depth 1)
''    node      = PROC( function( a as int ) as int )  (new function pointer subtype)
''    t = VAR( f as function( a as int ) as int )      (adjusted AST: f turned into function pointer)
''
'' Example 2:
''
''         int (*(*f)(int a))(int b);
'' depth 2:       ^^
''    innerprocptrdtype = TYPE_PROC                    (unused)
''    procptrdtype      = typeAddrOf( TYPE_PROC )
''    innernode = NULL
''    node      = NULL
''    t = VAR( f as int ptr ptr )    (f as int ptr ptr for now, in case it's just "int (*(*f));")
''
''         int (*(*f)(int a))(int b);
'' depth 1:     ^^  ^^^^^^^^
''    innerprocptrdtype = typeAddrOf( TYPE_PROC )      (passed up from depth 2)
''    procptrdtype      = typeAddrOf( TYPE_PROC )
''    innernode = VAR( f as int ptr ptr )              (passed up from depth 2)
''    node      = PROC( function( a as int ) as int ptr )  (new function pointer subtype,
''                                                     result = int ptr for now, in case it's
''                                                     just "int (*(*f)(int a));")
''    t = VAR( f as function( a as int ) as int ptr )  (adjusted AST: f turned into function pointer)
''
''         int (*(*f)(int a))(int b);
'' depth 0:    ^            ^^^^^^^^
''    innerprocptrdtype = typeAddrOf( TYPE_PROC )          (passed up from depth 1)
''    procptrdtype      = TYPE_PROC (unused)
''    innernode = PROC( function( a as int ) as int ptr )  (passed up from depth 1)
''    node      = PROC( function( b as int ) as int )      (new function pointer subtype)
''    t = VAR( f as function( a as int ) as function( b as int ) as int )
''                               (adjusted AST: f's subtype (innernode) turned into function pointer,
''                               i.e. the f function pointer now has a function pointer as its function
''                               result type, instead of "int ptr")
''
''
'' __attribute__((...)) parsing stuff:
''
'' These are all the same:
''    __attribute__((stdcall)) void (*p)(int a);
''    void (*p)(int a) __attribute__((stdcall));
''    void (__attribute__((stdcall)) *p)(int a);
''    void (* __attribute__((stdcall)) p)(int a);
''    extern p as sub stdcall( byval a as long )
'' i.e. the stdcall attribute goes to the procptr subtype, no matter whether
'' it appears in the toplevel declarator (the proc) or the nested declarator
'' (the pointer var).
''
'' Here the stdcall goes to the proc that's being declared, not to its result type:
''    __attribute__((stdcall)) void (*f(int a))(int b);
''    void (*f(int a))(int b) __attribute__((stdcall));
''    declare function f stdcall( byval a as long ) as sub cdecl( byval b as long )
''
'' Here the stdcall goes to the proc's result procptr subtype, not the proc itself:
''    void (__attribute__((stdcall)) *f(int a))(int b);
''    declare function f cdecl( byval a as long ) as sub stdcall( byval b as long )
''
'' This proc returns a pointer to the above one:
''    void (__attribute__((stdcall)) *(*f(int a))(int b))(int c);
''                                      ^^^^^^^^
''                                    ^^        ^^^^^^^^
''          ^^^^^^^^^^^^^^^^^^^^^^^^^^                  ^^^^^^^^
''    ^^^^^^                                                    ^
''    declare function f cdecl  ( byval a as long ) as _
''            function   cdecl  ( byval b as long ) as _
''            sub        stdcall( byval c as long )
''
'' Here the stdcall still goes to the proc that's being declared:
''    __attribute__((stdcall)) void (*(*f(int a))(int b))(int c);
''    declare function f stdcall( byval a as long ) as _
''            function   cdecl  ( byval b as long ) as _
''            sub        cdecl  ( byval c as long )
''
'' Here the stdcall still goes to the proc's result type:
''    void (*(__attribute__((stdcall)) *f(int a))(int b))(int c);
''    declare function f cdecl  ( byval a as long ) as _
''            function   stdcall( byval b as long ) as _
''            sub        cdecl  ( byval c as long )
''
'' I.e. attributes from the base type go to the inner most declarator (which
'' ends up defining the toplevel object, a proc or var), while attributes from
'' declarators go to the nodes for those declarators.
''
'' More about function pointers because they seem to be more complicated, these
'' are all ok:
''
''    __attribute__((stdcall)) void (*f(int a))(int b);         // proc
''    declare function f stdcall( byval a as long ) as sub cdecl( byval b as long )
''
''    extern __attribute__((stdcall)) void (*(*p)(int a))(int b) = f;  // ptr to it
''    extern void (*(__attribute__((stdcall)) *p)(int a))(int b) = f;  // same thing, apparently
''    extern p as function stdcall( byval a as long ) as sub cdecl( byval b as long )
''
'' I.e. we see again that for procptr vars, the attributes from toplevel and
'' inner-most declarators have the same effect - they go to the procptr subtype
'' in both cases.
''
''    void (__attribute__((stdcall)) *f(int a))(int b);         // different proc
''    declare function f cdecl( byval a as long ) as sub stdcall( byval b as long )
''
''    extern void (__attribute__((stdcall)) *(*p)(int a))(int b) = f;  // ptr to it
''    extern p as function cdecl( byval a as long ) as sub stdcall( byval b as long )
''
'' Here the stdcall is in the middle declarator and goes to the proc type
'' corresponding to it, the procptr's result type.
''
private function cDeclarator _
	( _
		byval nestlevel as integer, _
		byval astclass as integer, _
		byval outerdtype as integer, _
		byval basesubtype as ASTNODE ptr, _
		byval basegccattribs as integer, _
		byref node as ASTNODE ptr, _
		byref procptrdtype as integer, _
		byref gccattribs as integer _
	) as ASTNODE ptr

	var dtype = outerdtype
	var innerprocptrdtype = TYPE_PROC
	var innergccattribs = 0
	procptrdtype = TYPE_PROC
	gccattribs = 0

	'' __ATTRIBUTE__((...))
	''
	'' Note: __attribute__'s behind the base type are handled by cBaseType()
	'' already because they apply to the whole declaration:
	''    int __attribute__((stdcall)) f1(void), f2(void);
	'' both should be stdcall.
	''
	'' But this is still here, to handle __attribute__'s appearing in
	'' nested declarators:
	''    int (__attribute__((stdcall)) f1)(void);
	'' or at the front of follow-up declarators in a declaration:
	''    int f1(void), __attribute__((stdcall)) f2(void);
	''
	cGccAttributeList( gccattribs )

	'' Pointers: ('*')*
	while( cMatch( TK_STAR ) and c.parseok )
		procptrdtype = typeAddrOf( procptrdtype )
		dtype = typeAddrOf( dtype )

		'' (CONST|RESTRICT|__ATTRIBUTE__((...)))*
		while( c.parseok )
			'' __ATTRIBUTE__((...))
			cGccAttributeList( gccattribs )

			select case( tkGet( c.x ) )
			case KW_CONST
				procptrdtype = typeSetIsConst( procptrdtype )
				dtype = typeSetIsConst( dtype )
				c.x += 1

			case KW_RESTRICT, KW___RESTRICT, KW___RESTRICT__
				'' The restrict keyword is not interesting for FB bindings, just ignore
				c.x += 1

			case else
				exit while
			end select
		wend
	wend

	dim as ASTNODE ptr t, innernode

	''    '(' Declarator ')'    |    [Identifier]

	'' Special case for parameters:
	'' * They can be anonymous, and thus a '(' can indicate either a
	''   parenthesized identifier or a parameter list. It depends on the
	''   token behind the '(' - if it's a data type or a ')' then it's a
	''   parameter list.
	'' * If it's a parameter list, it can be parenthesized, even multiple
	''   times. This isn't possible with "normal" function declarations...
	var paramlistnesting = 0
	if( astclass = ASTCLASS_PARAM ) then
		var y = c.x
		while( tkGet( y ) = TK_LPAREN )
			y += 1
		wend
		if( hIsDataType( y ) or (tkGet( y ) = TK_RPAREN) ) then
			paramlistnesting = y - c.x
		end if
	end if

	'' '(' for declarator?
	if( (tkGet( c.x ) = TK_LPAREN) and (paramlistnesting = 0) ) then
		c.x += 1

		t = cDeclarator( nestlevel + 1, astclass, dtype, basesubtype, 0, innernode, innerprocptrdtype, innergccattribs )

		'' ')'
		cExpectMatch( TK_RPAREN, "for '(...)' parenthesized declarator" )
	else
		'' [Identifier]
		'' An identifier must exist, except for parameters/types, and
		'' in fact for types there mustn't be an id.
		dim as string id
		if( astclass <> ASTCLASS_DATATYPE ) then
			if( tkGet( c.x ) = TK_ID ) then
				id = *tkSpellId( c.x )
				c.x += 1
			else
				if( astclass <> ASTCLASS_PARAM ) then
					cError( "expected identifier for the symbol declared in this declaration" + tkButFound( c.x ) )
					id = "<error-recovery>"
				end if
			end if
		end if

		t = astNew( astclass, id )
		astSetType( t, dtype, basesubtype )
	end if

	node = t

	select case( tkGet( c.x ) )
	'' ('[' [ArrayElements] ']')*
	case TK_LBRACKET
		'' Can't allow arrays on everything - currently, it's only
		'' handled for vars/fields/params/typedefs
		if( node->class = ASTCLASS_DATATYPE ) then
			cError( "TODO: arrays not supported here yet" )
		end if

		assert( node->array = NULL )
		node->array = astNew( ASTCLASS_ARRAY )

		'' For each array dimension...
		do
			'' '['
			c.x += 1

			var d = astNew( ASTCLASS_DIMENSION )

			'' Just '[]'?
			if( tkGet( c.x ) = TK_RBRACKET ) then
				d->expr = astNew( ASTCLASS_ELLIPSIS )
			else
				d->expr = cExpression( )
			end if

			astAppend( node->array, d )

			'' ']'
			cExpectMatch( TK_RBRACKET, "to close this array dimension declaration" )

			'' '['? (next dimension)
		loop while( (tkGet( c.x ) = TK_LBRACKET) and c.parseok )

	'' ':' <bits>
	case TK_COLON
		if( node->class <> ASTCLASS_FIELD ) then
			cError( "bitfields not supported here" )
		end if
		c.x += 1

		node->bits = cExpression( )

	'' '(' ParamList ')'
	case TK_LPAREN
		if( paramlistnesting = 0 ) then
			paramlistnesting = 1
		end if
		c.x += paramlistnesting

		'' Parameters turn a vardecl/fielddecl into a procdecl,
		'' unless they're for a procptr type.
		if( innerprocptrdtype <> TYPE_PROC ) then
			'' There were '()'s above and the recursive
			'' cDeclarator() call found pointers/CONSTs,
			'' these parameters are for a function pointer.
			''
			'' Whichever object should become the function pointer,
			'' its dtype/subtype must be adjusted accordingly.
			'' For the subtype, a new PROC node is created, which
			'' will hold the parameters etc. found at this level.

			'' New PROC node for the function pointer's subtype
			node = astNew( ASTCLASS_PROC )
			astSetType( node, dtype, basesubtype )
			if( c.filterout = FALSE ) then
				api->need_externblock = TRUE
			end if

			'' Turn the object into a function pointer
			innernode->dtype = innerprocptrdtype
			innernode->subtype = node

			innerprocptrdtype = TYPE_PROC
		else
			select case( t->class )
			'' A plain symbol, not a pointer, becomes a function
			case ASTCLASS_VAR, ASTCLASS_FIELD
				t->class = ASTCLASS_PROC
				if( c.filterout = FALSE ) then
					api->need_externblock = TRUE
				end if

			'' Anything else though (typedefs, params, type casts...)
			'' with params isn't turned into a proc, but just has function type.
			case else
				node = astNew( ASTCLASS_PROC )
				astSetType( node, dtype, basesubtype )
				if( c.filterout = FALSE ) then
					api->need_externblock = TRUE
				end if
				t->dtype = TYPE_PROC
				t->subtype = node
			end select
		end if

		'' Just '(void)'?
		if( (tkGet( c.x ) = KW_VOID) and (tkGet( c.x + 1 ) = TK_RPAREN) ) then
			'' VOID
			c.x += 1
		'' Not just '()'?
		elseif( tkGet( c.x ) <> TK_RPAREN ) then
			assert( node->class = ASTCLASS_PROC )
			cPush( node, TRUE, TRUE )
			cParamDeclList( )
			cPop( )
		end if

		'' ')'
		while( paramlistnesting > 0 )
			cExpectMatch( TK_RPAREN, "to close parameter list in function declaration" )
			paramlistnesting -= 1
		wend
	end select

	'' __ATTRIBUTE__((...))
	var endgccattribs = 0
	cGccAttributeList( endgccattribs )

	if( nestlevel > 0 ) then
		'' __attribute__'s from this level should always be passed up
		gccattribs or= endgccattribs

		'' Pass innerprocptrdtype and innergccattribs up again if they
		'' weren't used up on this level
		if( innerprocptrdtype <> TYPE_PROC ) then
			gccattribs or= innergccattribs
			procptrdtype = typeMultAddrOf( procptrdtype, typeGetPtrCount( innerprocptrdtype ) ) or _
								typeGetConst( innerprocptrdtype )
		else
			node->attrib or= innergccattribs
		end if
	else
		'' At toplevel nothing can be passed up, everything must be assigned.
		'' __attribute__'s from the base type go to the toplevel symbol
		'' that's being declared. If it's a function pointer, then callconv
		'' attributes go to the proc subtype, not the procptr var.

		basegccattribs or= gccattribs or endgccattribs

		if( (typeGetDt( t->dtype ) = TYPE_PROC) and (t->class <> ASTCLASS_PROC) ) then
			assert( t->subtype->class = ASTCLASS_PROC )
			t->subtype->attrib or= basegccattribs and ASTATTRIB__CALLCONV
			t->attrib or= basegccattribs and (not ASTATTRIB__CALLCONV)
		else
			t->attrib or= basegccattribs
		end if

		node->attrib or= innergccattribs

		'' dllimport implies extern, and isn't allowed together with static
		if( t->attrib and ASTATTRIB_DLLIMPORT ) then
			if( t->attrib and ASTATTRIB_STATIC ) then
				cError( "static dllimport" )
				t->attrib and= not ASTATTRIB_STATIC
			end if
			t->attrib or= ASTATTRIB_EXTERN
		end if

		hPostprocessDeclarator( t )
	end if

	function = t
end function

'' Data type parsing for cast operations and sizeof():
''    BaseType Declarator
'' Parsing just the base type isn't enough, because it could be a function
'' pointer cast with parameter list etc. We need to do full declarator parsing
'' to handle that.
private function cDataType( ) as ASTNODE ptr
	dim as integer dtype, gccattribs
	dim as ASTNODE ptr subtype
	cBaseType( dtype, subtype, gccattribs, ASTCLASS_DATATYPE )
	function = cDeclarator( 0, ASTCLASS_DATATYPE, dtype, subtype, gccattribs, NULL, 0, 0 )
end function

''
'' Generic 'type *a, **b;' parsing, used for vars/fields/protos/params/typedefs
'' ("multiple declaration" syntax)
''    int i;
''    int a, b, c;
''    int *a, ***b, c;
''    int f(void);
''    int (*procptr)(void);
''    struct UDT { int a; };  (special case for BaseType only)
''
'' Declaration = GccAttributeList BaseType Declarator (',' Declarator)* [';']
''
private sub cDeclaration( byval astclass as integer, byval gccattribs as integer )
	'' BaseType
	dim as integer dtype
	dim as ASTNODE ptr subtype
	cBaseType( dtype, subtype, gccattribs, astclass )

	'' Special case for standalone struct/union/enum declarations:
	if( typeGetDtAndPtr( dtype ) = TYPE_UDT ) then
		'' Tag declaration with body?
		''    STRUCT|UNION|ENUM Identifier '{' ... '}' ';'
		'' Useless tag declaration?
		''    STRUCT|UNION|ENUM Identifier ';'
		select case( subtype->class )
		case ASTCLASS_STRUCT, ASTCLASS_UNION, ASTCLASS_ENUM
			'' ';'?
			if( cMatch( TK_SEMI ) ) then
				exit sub
			end if
		end select
	end if

	var require_semi = TRUE

	'' ... (',' ...)*
	var declarator_count = 0
	do
		declarator_count += 1
		var n = cDeclarator( 0, astclass, dtype, subtype, gccattribs, NULL, 0, 0 )

		'' filterout vs add_to_ast:
		'' Declarations that are going to be filtered out must still be added to the AST for
		'' symbol renaming (astFixIds()) which will be done after C parsing. This way the
		'' renaming produces the same result no matter whether some files are being filtered
		'' out or not.
		'' add_to_ast on the other hand is for C declarations which we're going to
		'' solve out right here as if they never existed (e.g. because they can't
		'' be translated to FB).
		var add_to_namespace = TRUE
		var add_to_ast = not c.filterout

		select case( n->class )
		case ASTCLASS_PROC
			if( c.filterout = FALSE ) then
				api->need_externblock = TRUE
			end if

			'' Should this procedure be removed?
			if( hashContains( @frog.idopt(OPT_REMOVEPROC), n->text, hashHash( n->text ) ) ) then
				add_to_namespace = FALSE
				add_to_ast = FALSE
			end if

		case ASTCLASS_VAR
			if( c.filterout = FALSE ) then
				api->need_externblock or= ((n->attrib and ASTATTRIB_EXTERN) <> 0)
				api->need_externblock or= ((n->attrib and ASTATTRIB_STATIC) = 0)
			end if

		case ASTCLASS_TYPEDEF
			'' Don't preserve array/function typedefs
			add_to_ast and= (n->array = NULL)
			select case( typeGetDtAndPtr( n->dtype ) )
			case TYPE_PROC
				add_to_ast = FALSE

			'' Is this typedef just an alias for a struct tag id?
			case TYPE_UDT
				select case( n->subtype->class )
				case ASTCLASS_STRUCT, ASTCLASS_UNION, ASTCLASS_ENUM
					'' Was it an unnamed struct (which was given a generated id in cTag())?
					if( n->subtype->attrib and ASTATTRIB_GENERATEDID ) then
						'' They should be in the same scope/namespace,
						'' since it's an unnamed struct, that must have been
						'' defined within the typedef declaration... (being unnamed,
						'' it couldn't possibly be looked up from a parent scope)
						''
						'' This means we have something like:
						''    typedef struct { ... } A;
						'' bad translation:
						''    type generatedid
						''        ...
						''    end type
						''    type A as generatedid
						'' good translation (typedef dropped):
						''    type A
						''        ...
						''    end type
						add_to_ast = FALSE

						'' Rename the struct to the same id as the typedef
						astSetText( n->subtype, n->text )
						n->subtype->attrib and= not ASTATTRIB_GENERATEDID

						'' Re-add to scope under the new id (we don't remove the hashtb entry under the old id,
						'' but that's ok, the id string and ASTNODE pointers both stay valid, and it's just the
						'' generatedid anyways which will never be looked up...)
						cAddSymbol( n->subtype )

						'' We still have to track the typedef symbol to be able to
						'' look it up when it's used in the code. But whenever that
						'' happens, we can solve the typedef out, i.e. reference its
						'' subtype instead of itself.
						n->attrib or= ASTATTRIB_SOLVEOUT

					'' Filter out "redundant" typedefs, for example:
					''    typedef struct T T;
					elseif( *astGetOrigId( n ) = *astGetOrigId( n->subtype ) ) then
						add_to_ast = FALSE

						'' ditto
						n->attrib or= ASTATTRIB_SOLVEOUT
					end if
				end select
			end select
		end select

		if( c.parseok ) then
			if( n->text ) then
				var existing = cLocalNameLookup( n->text )
				if( existing ) then
					'' Declared symbol already exists, so this most likely is a redeclaration.
					'' Otherwise it would be invalid C.
					if( frog.syntaxonly = FALSE ) then
						if( existing->class = n->class ) then
							select case( n->class )
							case ASTCLASS_PARAM, ASTCLASS_FIELD
								'' These can't be redeclared even if the signatures match
								cOops( "duplicate definition: " + astDumpPrettyDecl( n ) )
							end select
						end if

						if( existing->class = ASTCLASS_TEXT ) then
							'' There can already be a TEXT node (undeclared id) in case it was used in a #define body
							'' before being declared. If so, we want to turn the TEXT into a proper declaration.
							astCopy( existing, n )
						else
							'' We don't check that the signatures are really the same, because that's quite some work
							'' and most likely never useful/needed. For example, doing the check requires resolving typedefs
							'' to their real type, but we may not be able to do that due to incomplete input.
							'' Also, sometimes declarations really aren't exactly the same, but are accepted by the C compiler;
							'' e.g. one has some __attribute__ and the other doesn't.
							add_to_ast = FALSE
						end if
						n = existing
					end if
					add_to_namespace = FALSE
				end if
			else
				'' Anonymous parameter
				add_to_namespace = FALSE
			end if

			if( add_to_namespace ) then
				cAddSymbol( n )
			end if
		end if

		if( hCanHaveInitializer( n ) ) then
			'' ['=' Initializer]
			if( cMatch( TK_EQ ) ) then
				assert( n->expr = NULL )
				n->expr = cExpressionOrInitializer( )

				'' If it's an array, then it must be an array initializer (or a string literal),
				'' not a struct initializer
				if( n->array ) then
					if( n->expr->class = ASTCLASS_STRUCTINIT ) then
						n->expr->class = ASTCLASS_ARRAYINIT
					end if
				end if
			end if
		end if

		if( c.parseok and add_to_ast ) then
			cAppendNode( n )
		end if

		'' Parameters can't have commas and more identifiers,
		'' and don't need the ';' either.
		if( n->class = ASTCLASS_PARAM ) then
			require_semi = FALSE
			exit do
		end if

		'' '{', procedure body?
		if( (n->class = ASTCLASS_PROC) and (tkGet( c.x ) = TK_LBRACE) ) then
			'' A procedure with body must be the first and only
			'' declarator in the declaration.
			if( declarator_count = 1 ) then
				'' Mustn't have a body yet
				if( n->expr ) then
					cOops( "function body redefinition for " + astDumpPrettyDecl( n ) )
				end if

				var originalparseok = c.parseok

				n->expr = cScope( n )

				if( frog.nofunctionbodies ) then
					c.parseok = originalparseok
					n->expr = NULL
				end if

				require_semi = FALSE
				exit do
			end if
		end if

		'' ','?
	loop while( cMatch( TK_COMMA ) and c.parseok )

	if( require_semi ) then
		'' ';'
		cExpectMatch( TK_SEMI, "to finish this declaration" )
	end if
end sub

'' Variable/procedure declarations
''    GccAttributeList [EXTERN|STATIC] Declaration
private sub cVarOrProcDecl( byval is_local as integer )
	'' __ATTRIBUTE__((...))
	var gccattribs = 0
	cGccAttributeList( gccattribs )

	'' [EXTERN|STATIC]
	select case( tkGet( c.x ) )
	case KW_EXTERN
		gccattribs or= ASTATTRIB_EXTERN
		c.x += 1
	case KW_STATIC
		gccattribs or= ASTATTRIB_STATIC
		c.x += 1
	end select

	if( c.namespacelevel > 0 ) then
		gccattribs or= ASTATTRIB_LOCAL
	end if

	'' Declaration. Assume that it's a variable for now; the declarator
	'' parser may turn it into a procedure if it has parameters.
	cDeclaration( ASTCLASS_VAR, gccattribs )
end sub

'' Expression statement: Assignments, function calls, i++, etc.
private sub cExpressionStatement( )
	var expr = cExpression( )
	if( c.parseok ) then
		cAppendNode( expr )
	end if

	'' ';'?
	cExpectMatch( TK_SEMI, "(end of expression statement)" )
end sub

'' RETURN Expression ';'
private sub cReturn( )
	'' RETURN
	assert( tkGet( c.x ) = KW_RETURN )
	c.x += 1

	'' Expression
	var expr = cExpression( )
	if( c.parseok ) then
		cAppendNode( astNew( ASTCLASS_RETURN, expr ) )
	end if

	'' ';'
	cExpectMatch( TK_SEMI, "(end of statement)" )
end sub

'' '{ ... }' statement block
'' Using cBody() to allow the constructs in this scope block to be parsed
'' separately. If we can't parse one of them, then only that one will become an
'' unknown construct. The rest of the scope can potentially be parsed fine.
private function cScope( byval proc as ASTNODE ptr ) as ASTNODE ptr
	'' '{'
	assert( tkGet( c.x ) = TK_LBRACE )
	c.x += 1

	var t = astNew( ASTCLASS_SCOPEBLOCK )
	cPush( t, TRUE, TRUE )

	'' If parsing a proc body, add the parameters to the scope too,
	'' so they can be looked up
	if( proc ) then
		var param = proc->head
		while( param )
			'' Not an anonymous parameter?
			if( param->text ) then
				cAddCHashTbEntry( param )
			end if
			param = param->next
		wend
	end if

	cBody( )

	cPop( )

	'' '}'
	cExpectMatch( TK_RBRACE, "to close compound statement" )

	function = t
end function

private sub cConstruct( )
	'' '#'?
	if( (tkGet( c.x ) = TK_HASH) and (tkGetExpansionLevel( c.x ) = 0) ) then
		c.x += 1

		var ok = FALSE

		if( tkGet( c.x ) = TK_ID ) then
			select case( *tkSpellId( c.x ) )
			case "define"
				cDefine( )
				ok = TRUE
			case "pragma"
				c.x += 1

				select case( tkSpell( c.x ) )
				case "pack"
					ok = cPragmaPack( )
				case "comment"
					cPragmaComment( )
					ok = TRUE
				end select
			end select
		end if

		if( ok = FALSE ) then
			cError( "unknown CPP directive" )
		end if

		exit sub
	end if

	if( c.blocks(c.blocklevel).context->class = ASTCLASS_ENUM ) then
		cEnumConst( )
		exit sub
	end if

	select case( tkGet( c.x ) )
	case KW_TYPEDEF
		cTypedef( )
		exit sub
	case TK_SEMI
		'' Ignore standalone ';'
		c.x += 1
		exit sub
	end select

	select case( c.blocks(c.blocklevel).context->class )
	case ASTCLASS_STRUCT, ASTCLASS_UNION
		'' Field declaration
		cDeclaration( ASTCLASS_FIELD, 0 )

	case ASTCLASS_SCOPEBLOCK
		'' Disambiguate: local declaration vs. expression
		'' If it starts with a data type, __attribute__, or 'static',
		'' then it must be a declaration.
		select case( tkGet( c.x ) )
		case KW_STATIC
			cVarOrProcDecl( TRUE )
		case KW_RETURN
			cReturn( )
		case else
			if( hIsDataTypeOrAttribute( c.x ) ) then
				cVarOrProcDecl( TRUE )
			else
				cExpressionStatement( )
			end if
		end select

	case else
		cVarOrProcDecl( FALSE )
	end select
end sub

private sub cUpdateFilterOut( )
	c.filterout = ((tkGetFlags( c.x ) and TKFLAG_FILTEROUT) <> 0)
end sub

private sub cPopRemainingScopes( byval blocklevel0 as integer )
	'' Close scopes that were left open due to the parsing error
	while( c.blocklevel > blocklevel0 )
		cPop( )
	wend
end sub

private sub cBody( )
	do
		select case( tkGet( c.x ) )
		case TK_EOF
			exit do

		'' End of #define body
		case TK_EOL
			assert( c.parentdefine )
			exit do

		'' '}' (end of block)
		case TK_RBRACE
			if( c.blocklevel > 0 ) then
				exit do
			end if
		end select

		if( c.blocklevel = 0 ) then
			cUpdateFilterOut( )
		end if

		var begin = c.x
		var blocklevel0 = c.blocklevel

		cConstruct( )

		if( c.parseok = FALSE ) then
			c.parseok = TRUE

			'' Skip current construct and preserve its tokens in
			'' an UNKNOWN node
			c.x = hSkipConstruct( begin, FALSE )
			if( c.filterout = FALSE ) then
				cAppendNode( astNewUNKNOWN( begin, c.x - 1 ) )
			end if
			cPopRemainingScopes( blocklevel0 )
		end if

		assert( c.blocklevel = blocklevel0 )
	loop
end sub

sub cMain( )
	cBody( )

	'' Process the #define bodies which weren't parsed yet
	for i as integer = 0 to c.defbodycount - 1
		with( c.defbodies[i] )
			assert( c.blocklevel = 0 )
			c.parseok = TRUE
			c.x = .xbodybegin
			cUpdateFilterOut( )

			'' Parse #define body
			var add_to_ast = TRUE
			cParseDefBody( .n, add_to_ast )

			cPopRemainingScopes( 0 )

			if( (not add_to_ast) and (not c.filterout) ) then
				astRemove( api->ast, .n )
			end if
		end with
	next
end sub
