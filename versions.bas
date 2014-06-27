'' Version expression/block handling functions

#include once "fbfrog.bi"

type DECLNODE
	n as ASTNODE ptr  '' The declaration at that index
	v as ASTNODE ptr  '' Parent VERSION node of the declaration
end type

type DECLTABLE
	array	as DECLNODE ptr
	count	as integer
	room	as integer
end type

private sub decltableAdd _
	( _
		byval table as DECLTABLE ptr, _
		byval n as ASTNODE ptr, _
		byval v as ASTNODE ptr _
	)

	if( table->count = table->room ) then
		table->room += 256
		table->array = reallocate( table->array, table->room * sizeof( DECLNODE ) )
	end if

	with( table->array[table->count] )
		.n = n
		.v = v
	end with

	table->count += 1

end sub

private sub decltableInit _
	( _
		byval table as DECLTABLE ptr, _
		byval code as ASTNODE ptr _
	)

	table->array = NULL
	table->count = 0
	table->room = 0

	'' Add each declaration node from the AST to the table
	'' For each VERBLOCK...
	assert( code->class = ASTCLASS_GROUP )
	var verblock = code->head
	while( verblock )
		assert( verblock->class = ASTCLASS_VERBLOCK )

		'' For each declaration in that VERBLOCK...
		var decl = verblock->head
		while( decl )
			decltableAdd( table, decl, verblock->expr )
			decl = decl->next
		wend

		verblock = verblock->next
	wend
end sub

private sub decltableEnd( byval table as DECLTABLE ptr )
	deallocate( table->array )
end sub

''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''

'' Just as the whole file AST is wrapped in a verblock, the fields of struct
'' should be aswell, so hMergeStructsManually() doesn't have to handle both
'' cases of "fresh still unwrapped fields" and "already wrapped from previous
'' merge", but only the latter.
private sub hWrapStructFieldsInVerblocks _
	( _
		byval code as ASTNODE ptr, _
		byval version as ASTNODE ptr _
	)

	var i = code->head
	while( i )
		hWrapStructFieldsInVerblocks( i, version )
		i = i->next
	wend

	select case( code->class )
	case ASTCLASS_STRUCT, ASTCLASS_UNION, ASTCLASS_ENUM
		var newfields = astNewVERBLOCK( astClone( version ), NULL, astBuildGROUPFromChildren( code ) )
		astRemoveChildren( code )
		astAppend( code, newfields )
	end select

end sub

function astWrapFileInVerblock _
	( _
		byval code as ASTNODE ptr, _
		byval version as ASTNODE ptr _
	) as ASTNODE ptr
	hWrapStructFieldsInVerblocks( code, version )
	function = astNewVERBLOCK( astClone( version ), NULL, code )
end function

private sub astAppendVerblock _
	( _
		byval n as ASTNODE ptr, _
		byval version1 as ASTNODE ptr, _
		byval version2 as ASTNODE ptr, _
		byval child as ASTNODE ptr _
	)

	'' If the tree's last VERBLOCK has the same version numbers, then
	'' just add the new children nodes to that instead of opening a new
	'' separate VERBLOCK.
	if( n->tail ) then
		if( n->tail->class = ASTCLASS_VERBLOCK ) then
			var tmp = astNewGROUP( astClone( version1 ) )
			astAppendUnique( tmp, astClone( version2 ) )

			var ismatch = astIsEqual( n->tail->expr, tmp )

			astDelete( tmp )

			if( ismatch ) then
				astAppend( n->tail, child )
				exit sub
			end if
		end if
	end if

	astAppend( n, astNewVERBLOCK( version1, version2, child ) )
end sub

private sub hAddDecl _
	( _
		byval c as ASTNODE ptr, _
		byval array as DECLNODE ptr, _
		byval i as integer _
	)

	astAppendVerblock( c, astClone( array[i].v ), NULL, astClone( array[i].n ) )

end sub

''
'' See also hTurnCallConvIntoExternBlock():
''
'' Procdecls with callconv covered by the Extern block are given the
'' ASTATTRIB_HIDECALLCONV flag.
''
'' If we're merging two procdecls here, and they both have ASTATTRIB_HIDECALLCONV,
'' then they can be emitted without explicit callconv, as the Extern blocks will
'' take care of that and remap the callconv as needed. In this case, the merged
'' node shouldn't have any callconv flag at all, but only ASTATTRIB_HIDECALLCONV.
'' hAstLCS() must be given the proper option flags for astIsEqual() to allow this.
''
'' If merging two procdecls and only one side has ASTATTRIB_HIDECALLCONV, then they
'' must have the same callconv, otherwise the hAstLCS()'s astIsEqual() wouldn't
'' have treated them as equal. In this case the callconv must be preserved on the
'' merged node, so it will be emitted explicitly, since the Extern blocks don't
'' cover it. ASTATTRIB_HIDECALLCONV shouldn't be preserved in this case.
''
'' The same applies to procptr subtypes, and to be able to handle them, this
'' function is recursive.
''
private sub hFindCommonCallConvsOnMergedDecl _
	( _
		byval mdecl as ASTNODE ptr, _
		byval adecl as ASTNODE ptr, _
		byval bdecl as ASTNODE ptr _
	)

	assert( mdecl->class = adecl->class )
	assert( adecl->class = bdecl->class )

	if( mdecl->class = ASTCLASS_PROC ) then
		if( ((adecl->attrib and ASTATTRIB_HIDECALLCONV) <> 0) and _
		    ((bdecl->attrib and ASTATTRIB_HIDECALLCONV) <> 0) ) then
			mdecl->attrib and= not (ASTATTRIB_CDECL or ASTATTRIB_STDCALL)
			assert( mdecl->attrib and ASTATTRIB_HIDECALLCONV ) '' was preserved by astClone() already
		elseif( ((adecl->attrib and ASTATTRIB_HIDECALLCONV) <> 0) or _
			((bdecl->attrib and ASTATTRIB_HIDECALLCONV) <> 0) ) then
			assert( (adecl->attrib and (ASTATTRIB_CDECL or ASTATTRIB_STDCALL)) = _
				(bdecl->attrib and (ASTATTRIB_CDECL or ASTATTRIB_STDCALL)) )
			mdecl->attrib and= not ASTATTRIB_HIDECALLCONV
			assert( (mdecl->attrib and (ASTATTRIB_CDECL or ASTATTRIB_STDCALL)) <> 0 ) '' ditto
		end if
	end if

	'' Don't forget the procptr subtypes
	if( typeGetDt( mdecl->dtype ) = TYPE_PROC ) then
		assert( typeGetDt( adecl->dtype ) = TYPE_PROC )
		assert( typeGetDt( bdecl->dtype ) = TYPE_PROC )
		hFindCommonCallConvsOnMergedDecl( mdecl->subtype, adecl->subtype, bdecl->subtype )
	end if

	var mchild = mdecl->head
	var achild = adecl->head
	var bchild = bdecl->head
	while( mchild )
		assert( achild )
		assert( bchild )

		hFindCommonCallConvsOnMergedDecl( mchild, achild, bchild )

		mchild = mchild->next
		achild = achild->next
		bchild = bchild->next
	wend
	assert( mchild = NULL )
	assert( achild = NULL )
	assert( bchild = NULL )
end sub

private sub hAddMergedDecl _
	( _
		byval c as ASTNODE ptr, _
		byval aarray as DECLNODE ptr, _
		byval ai as integer, _
		byval barray as DECLNODE ptr, _
		byval bi as integer _
	)

	var adecl = aarray[ai].n
	var bdecl = barray[bi].n

	'' "Merge" a and b by cloning a. They've compared equal in astIsEqual()
	'' so this works. Below we only need to cover a few additional cases
	'' where astIsEqual() is more permissive than a true equality check,
	'' which allows merging of a/b even if they're slightly different.
	'' This currently affects the calling convention only.
	'' In such cases, just cloning a isn't enough and some actual merging
	'' work is needed.
	var mdecl = astClone( adecl )

	hFindCommonCallConvsOnMergedDecl( mdecl, adecl, bdecl )

	astAppendVerblock( c, astClone( aarray[ai].v ), astClone( barray[bi].v ), mdecl )

end sub

''
'' Determine the longest common substring, by building an l x r matrix:
''
'' if l[i] = r[j] then
''     if( i-1 or j-1 would be out-of-bounds ) then
''         matrix[i][j] = 1
''     else
''         matrix[i][j] = matrix[i-1][j-1] + 1
''     end if
'' else
''     matrix[i][j] = 0
'' end if
''
'' 0 = characters not equal
'' 1 = characters equal
'' 2 = characters equal here, and also at one position to top/left
'' ...
'' i.e. the non-zero diagonal parts in the matrix determine the common
'' substrings.
''
'' Some examples:
''
''             Longest common substring:
''   b a a c           b a a c
'' a 0 1 1 0         a   1
'' a 0 1 2 0         a     2
'' b 1 0 0 0         b
'' c 0 0 0 1         c
''
''   c a a b           c a a b
'' a 0 1 1 0         a   1
'' a 0 1 2 0         a     2
'' b 1 0 0 3         b       3
'' c 1 0 0 0         c
''
''   b a b c           b a b c
'' c 0 1 0 1         c
'' a 0 1 0 0         a   1
'' b 1 0 2 0         b     2
'' c 0 0 0 3         c       3
''
private sub hAstLCS _
	( _
		byval larray as DECLNODE ptr, _
		byval lfirst as integer, _
		byval llast as integer, _
		byref llcsfirst as integer, _
		byref llcslast as integer, _
		byval rarray as DECLNODE ptr, _
		byval rfirst as integer, _
		byval rlast as integer, _
		byref rlcsfirst as integer, _
		byref rlcslast as integer, _
		byval equaloptions as integer _
	)

	var llen = llast - lfirst + 1
	var rlen = rlast - rfirst + 1
	var max = 0, maxi = 0, maxj = 0

	dim as integer ptr matrix = callocate( sizeof( integer ) * llen * rlen )

	for i as integer = 0 to llen-1
		for j as integer = 0 to rlen-1
			var newval = 0
			if( astIsEqual( larray[lfirst+i].n, rarray[rfirst+j].n, equaloptions ) ) then
				if( (i = 0) or (j = 0) ) then
					newval = 1
				else
					newval = matrix[(i-1)+((j-1)*llen)] + 1
				end if
			end if
			if( max < newval ) then
				max = newval
				maxi = i
				maxj = j
			end if
			matrix[i+(j*llen)] = newval
		next
	next

	deallocate( matrix )

	llcsfirst = lfirst + maxi - max + 1
	rlcsfirst = rfirst + maxj - max + 1
	llcslast  = llcsfirst + max - 1
	rlcslast  = rlcsfirst + max - 1
end sub

private function hMergeStructsManually _
	( _
		byval astruct as ASTNODE ptr, _
		byval bstruct as ASTNODE ptr _
	) as ASTNODE ptr

	''
	'' For example:
	''
	''     verblock 1                   verblock 2
	''         struct FOO                   struct FOO
	''             field a as integer           field a as integer
	''             field b as integer           field c as integer
	''
	'' should become:
	''
	''     version 1, 2
	''         struct FOO
	''             version 1, 2
	''                 field a as integer
	''             version 1
	''                 field b as integer
	''             version 2
	''                 field c as integer
	''
	'' instead of:
	''
	''     version 1
	''         struct FOO
	''             field a as integer
	''             field b as integer
	''     version 2
	''         struct FOO
	''             field a as integer
	''             field c as integer
	''

	var afields = astBuildGROUPFromChildren( astruct )
	var bfields = astBuildGROUPFromChildren( bstruct )

	#if 0
		print "afields:"
		astDump( afields, 1 )
		print "bfields:"
		astDump( bfields, 1 )
	#endif

	'' Merge both set of fields
	var fields = astMergeVerblocks( afields, bfields )

	'' Create a result struct with the new set of fields
	var cstruct = astCloneNode( astruct )
	astAppend( cstruct, fields )

	#if 0
		print "cstruct:"
		astDump( cstruct, 1 )
	#endif

	function = cstruct
end function

private sub hAstMerge _
	( _
		byval c as ASTNODE ptr, _
		byval aarray as DECLNODE ptr, _
		byval afirst as integer, _
		byval alast as integer, _
		byval barray as DECLNODE ptr, _
		byval bfirst as integer, _
		byval blast as integer, _
		byval btablecount as integer _
	)

	static reclevel as integer
	#if 0
		#define DEBUG( x ) print string( reclevel + 1, " " ) & x
	#else
		#define DEBUG( x )
	#endif

	DEBUG( "hAstMerge( reclevel=" & reclevel & ", a=" & afirst & ".." & alast & ", b=" & bfirst & ".." & blast & " )" )

	'' No longest common substring possible?
	if( afirst > alast ) then
		'' Add bfirst..blast to result
		DEBUG( "no LCS possible due to a, adding b as-is" )
		for i as integer = bfirst to blast
			hAddDecl( c, barray, i )
		next
		exit sub
	elseif( bfirst > blast ) then
		'' Add afirst..alast to result
		DEBUG( "no LCS possible due to b, adding a as-is" )
		for i as integer = afirst to alast
			hAddDecl( c, aarray, i )
		next
		exit sub
	end if

	'' Find longest common substring
	DEBUG( "searching LCS..." )
	dim as integer alcsfirst, alcslast, blcsfirst, blcslast
	hAstLCS( aarray, afirst, alast, alcsfirst, alcslast, _
	         barray, bfirst, blast, blcsfirst, blcslast, _
	         ASTEQ_IGNOREHIDDENCALLCONV or ASTEQ_IGNOREFIELDS or ASTEQ_IGNOREDUMMYIDSTRUCTS )
	DEBUG( "LCS: a=" & alcsfirst & ".." & alcslast & ", b=" & blcsfirst & ".." & blcslast )

	'' No LCS found?
	if( alcsfirst > alcslast ) then
		'' Add a first, then b. This order makes the most sense: keeping
		'' the old declarations at the top, add new ones to the bottom.
		DEBUG( "no LCS found, adding both as-is" )
		for i as integer = afirst to alast
			hAddDecl( c, aarray, i )
		next
		for i as integer = bfirst to blast
			hAddDecl( c, barray, i )
		next
		exit sub
	end if

	'' Do both sides have decls before the LCS?
	if( (alcsfirst > afirst) and (blcsfirst > bfirst) ) then
		'' Do LCS on that recursively
		DEBUG( "both sides have decls before LCS, recursing" )
		reclevel += 1
		hAstMerge( c, aarray, afirst, alcsfirst - 1, _
		              barray, bfirst, blcsfirst - 1, btablecount )
		reclevel -= 1
	elseif( alcsfirst > afirst ) then
		'' Only a has decls before the LCS; copy them into result first
		DEBUG( "only a has decls before LCS" )
		for i as integer = afirst to alcsfirst - 1
			hAddDecl( c, aarray, i )
		next
	elseif( blcsfirst > bfirst ) then
		'' Only b has decls before the LCS; copy them into result first
		DEBUG( "only b has decls before LCS" )
		for i as integer = bfirst to blcsfirst - 1
			hAddDecl( c, barray, i )
		next
	end if

	'' Add LCS
	DEBUG( "adding LCS" )
	assert( (alcslast - alcsfirst + 1) = (blcslast - blcsfirst + 1) )
	for i as integer = 0 to (alcslast - alcsfirst + 1)-1
		'' The LCS may include merged structs/unions/enums that were put
		'' into the LCS despite having different fields on both sides.
		''
		'' They should be merged recursively now, so the struct/union/enum itself
		'' can be common, while the fields/enumconsts may be version dependant.
		''
		'' (relying on structs/unions/enums to be allowed to match in the
		'' hAstLCS() call, even if they have different fields/enumconsts)
		var astruct = aarray[alcsfirst+i].n
		select case( astruct->class )
		case ASTCLASS_STRUCT, ASTCLASS_UNION, ASTCLASS_ENUM
			var aversion = aarray[alcsfirst+i].v
			var bstruct  = barray[blcsfirst+i].n
			var bversion = barray[blcsfirst+i].v
			assert( (bstruct->class = ASTCLASS_STRUCT) or _
			        (bstruct->class = ASTCLASS_UNION) or _
			        (bstruct->class = ASTCLASS_ENUM) )

			var cstruct = hMergeStructsManually( astruct, bstruct )

			'' Add struct to result tree, under both a's and b's version numbers
			astAppendVerblock( c, astClone( aversion ), astClone( bversion ), cstruct )

			if( astruct->text ) then
				if( strStartsWith( *astruct->text, DUMMYID_PREFIX ) ) then
					assert( *astruct->text <> *bstruct->text )
					assert( strStartsWith( *bstruct->text, DUMMYID_PREFIX ) )
					assert( *cstruct->text = *astruct->text )

					'' Two structs with dummy ids, being merged together.
					'' hMergeStructsManually() will have re-use a's id as id
					'' for the merged struct, and now we need to manually update
					'' all uses of b's id to now use a's id too so they'll be merged
					'' successfully (assuming merging walks through declarations in order
					'' like a single-pass compiler).

					for bi as integer = blcsfirst+i+1 to btablecount-1
						astReplaceSubtypes( barray[bi].n, ASTCLASS_TAGID, bstruct->text, ASTCLASS_TAGID, cstruct->text )
					next
				end if
			end if

		case else
			hAddMergedDecl( c, aarray, alcsfirst + i, barray, blcsfirst + i )
		end select
	next

	'' Do both sides have decls behind the LCS?
	if( (alcslast < alast) and (blcslast < blast) ) then
		'' Do LCS on that recursively
		DEBUG( "both sides have decls behind LCS, recursing" )
		reclevel += 1
		hAstMerge( c, aarray, alcslast + 1, alast, barray, blcslast + 1, blast, btablecount )
		reclevel -= 1
	elseif( alcslast < alast ) then
		'' Only a has decls behind the LCS
		DEBUG( "only a has decls behind LCS" )
		for i as integer = alcslast + 1 to alast
			hAddDecl( c, aarray, i )
		next
	elseif( blcslast < blast ) then
		'' Only b has decls behind the LCS
		DEBUG( "only b has decls behind LCS" )
		for i as integer = blcslast + 1 to blast
			hAddDecl( c, barray, i )
		next
	end if

end sub

function astMergeVerblocks _
	( _
		byval a as ASTNODE ptr, _
		byval b as ASTNODE ptr _
	) as ASTNODE ptr

	var c = astNewGROUP( )
	a = astNewGROUP( a )
	b = astNewGROUP( b )

	#if 0
		print "a:"
		astDump( a, 1 )
		print "b:"
		astDump( b, 1 )
	#endif

	'' Create a lookup table for each side, so the LCS algorithm can do
	'' index-based lookups in O(1) instead of having to cycle through the
	'' whole list of preceding nodes everytime which was terribly slow.
	dim atable as DECLTABLE
	dim btable as DECLTABLE

	decltableInit( @atable, a )
	decltableInit( @btable, b )

	hAstMerge( c, atable.array, 0, atable.count - 1, _
	              btable.array, 0, btable.count - 1, btable.count )

	decltableEnd( @btable )
	decltableEnd( @atable )

	#if 0
		print "c:"
		astDump( c, 1 )
	#endif

	function = c
end function

''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''

function astNewVERBLOCK _
	( _
		byval version1 as ASTNODE ptr, _
		byval version2 as ASTNODE ptr, _
		byval child as ASTNODE ptr _
	) as ASTNODE ptr

	var n = astNew( ASTCLASS_VERBLOCK, child )

	n->expr = astNewGROUP( version1 )
	astAppendUnique( n->expr, version2 )

	function = n
end function

function astCollectVersions( byval code as ASTNODE ptr ) as ASTNODE ptr
	var versions = astNewGROUP( )

	var i = code->head
	while( i )

		if( i->class = ASTCLASS_VERBLOCK ) then
			astAppendUnique( versions, astClone( i->expr ) )
		end if

		i = i->next
	wend

	function = versions
end function

'' Extract nodes corresponding to the wanted version only, i.e. all nodes except
'' those inside verblocks for other versions.
private function hGet1VersionOnly( byval code as ASTNODE ptr, byval v as ASTNODE ptr ) as ASTNODE ptr
	var result = astNewGROUP( )

	var i = code->head
	while( i )

		if( i->class = ASTCLASS_VERBLOCK ) then
			'' Nodes in verblocks are only added if the verblock matches the wanted version
			if( astGroupContains( i->expr, v ) ) then
				astCloneAppendChildren( result, i )
			end if
		else
			'' Unversioned nodes are added no matter what version we want
			astCloneAppend( result, i )
		end if

		i = i->next
	wend

	function = result
end function

private function hGet1TargetOnly( byval code as ASTNODE ptr, byval target as integer ) as ASTNODE ptr
	var result = astNewGROUP( )

	var i = code->head
	while( i )

		if( i->class = ASTCLASS_TARGETBLOCK ) then
			'' Nodes in targetblocks are only added if it's the wanted target
			if( (i->attrib and ASTATTRIB__ALLTARGET) = target ) then
				astCloneAppendChildren( result, i )
			end if
		else
			'' Non-OS-specific nodes are always used
			astCloneAppend( result, i )
		end if

		i = i->next
	wend

	function = result
end function

'' Extract only the nodes corresponding to one specific version & OS from the
'' given preset code.
function astGet1VersionAndTargetOnly _
	( _
		byval code as ASTNODE ptr, _
		byval version as ASTNODE ptr _
	) as ASTNODE ptr

	'' Remove the OS attrib from the version, so it can be matched against
	'' the original verblocks which aren't OS specific. The OS attrib is
	'' used to match OS blocks though.
	var v = astClone( version )
	var target = v->attrib and ASTATTRIB__ALLTARGET
	v->attrib and= not ASTATTRIB__ALLTARGET

	code = hGet1VersionOnly( code, v )
	code = hGet1TargetOnly( code, target )

	astDelete( v )

	function = code
end function

'' For each version in the version expression, if it exists multiple times,
'' combined with each target, then that can be folded to a single version each,
'' with all the target attributes.
''     verblock a.dos, a.linux, a.win32, b.dos, b.linux, b.win32, c.win32
'' becomes:
''     verblock a.(dos|linux|win32), b.(dos|linux|win32), c.win32
private sub hCombineVersionTargets( byval group as ASTNODE ptr )
	var i = group->head
	while( i )

		var nxt = i->next

		'' Find a second node with the same version but potentially
		'' different target attribute, and merge the two
		var j = nxt
		while( j )

			if( astIsEqual( i, j, ASTEQ_IGNORETARGET ) ) then
				i->attrib or= j->attrib and ASTATTRIB__ALLTARGET
				astRemove( group, j )
				nxt = i  '' Recheck i in case there are more duplicates
				exit while
			end if

			j = j->next
		wend

		i = nxt
	wend
end sub

'' If a version covers all targets then they can simply be forgotten,
'' because the check would always be true:
''     verblock a.(dos|linux|win32), b.(dos|linux|win32), c.win32
'' becomes:
''     verblock a, b, c.win32
private sub hRemoveFullTargets( byval group as ASTNODE ptr )
	var i = group->head
	while( i )
		if( (i->attrib and ASTATTRIB__ALLTARGET) = ASTATTRIB__ALLTARGET ) then
			i->attrib and= not ASTATTRIB__ALLTARGET
		end if
		i = i->next
	wend
end sub

'' If all versions in an expression use the same target, then that can be solved
'' out into a separate check to reduce the amount of checks generated in the end:
''     verblock a.(dos|win32), b.(dos|win32), c.(dos|win32)
'' becomes:
''     verblock a, b, c
''         targetblock dos|win32
'' (in many cases, the next step will then be able to solve out the verblock)
private function hExtractCommonTargets( byval group as ASTNODE ptr ) as integer
	var i = group->head
	if( i = NULL ) then exit function

	'' Get the target attribs of the first node...
	var targets = i->attrib and ASTATTRIB__ALLTARGET

	'' And compare each other node against it...
	do
		i = i->next
		if( i = NULL ) then exit do
		if( (i->attrib and ASTATTRIB__ALLTARGET) <> targets ) then exit function
	loop

	'' If the end of the list was reached without finding a mismatch,
	'' the target attrib(s) can be extracted. Remove attribs from each node:
	i = group->head
	do
		i->attrib and= not ASTATTRIB__ALLTARGET
		i = i->next
	loop while( i )

	'' Let the caller add the targetblock...
	function = targets
end function

'' Try to simplify each verblock's version expression
private sub hSimplifyVerblocks _
	( _
		byval code as ASTNODE ptr, _
		byval versions as ASTNODE ptr _
	)

	var i = code->head
	while( i )
		var nxt = i->next

		'' Handle nested verblocks inside structs
		hSimplifyVerblocks( i, versions )

		if( i->class = ASTCLASS_VERBLOCK ) then
			hCombineVersionTargets( i->expr )
			hRemoveFullTargets( i->expr )

			var extractedtargets = hExtractCommonTargets( i->expr )
			if( extractedtargets ) then
				'' Put this verblock's children into an targetblock,
				'' and add it in place of the children.
				var block = astNew( ASTCLASS_TARGETBLOCK )
				block->attrib or= extractedtargets
				astCloneAppendChildren( block, i )
				astRemoveChildren( i )
				astAppend( i, block )
			end if

			'' If a verblock covers all versions (without targets),
			'' then that can be solved out, since the check would always be true:
			''     verblock a, b, c
			''         <...>
			'' becomes:
			''     <...>
			if( astIsEqual( i->expr, versions ) ) then
				'' Remove the block but preserve its children
				astReplace( code, i, astBuildGROUPFromChildren( i ) )
			end if
		end if

		i = nxt
	wend

end sub

'' Structs/unions/enums can have nested verblocks (to allow fields to be versioned).
''     verblock 1
''         struct UDT
''             verblock 1
''                 field x as integer
'' can be simplified to:
''     verblock 1
''         struct UDT
''             field x as integer
private sub hSolveOutRedundantNestedVerblocks _
	( _
		byval code as ASTNODE ptr, _
		byval parentversions as ASTNODE ptr _
	)

	var i = code->head
	while( i )
		var nxt = i->next

		'' If a nested verblock covers >= of the parent verblock's versions,
		'' then its checking expression would always evaluate to true, so it's
		'' useless and can be removed.
		if( i->class = ASTCLASS_VERBLOCK ) then
			hSolveOutRedundantNestedVerblocks( i, i->expr )

			'' Has a parent?
			if( parentversions ) then
				'' Are all the parent's versions covered?
				if( astGroupContainsAllChildrenOf( i->expr, parentversions ) ) then
					'' Remove this verblock, preserve only its children
					astReplace( code, i, astBuildGROUPFromChildren( i ) )
				end if
			end if
		else
			hSolveOutRedundantNestedVerblocks( i, parentversions )
		end if

		i = nxt
	wend

end sub

'' Same for targetblocks
private sub hSolveOutRedundantNestedTargetblocks _
	( _
		byval code as ASTNODE ptr, _
		byval parenttargets as integer _
	)

	var i = code->head
	while( i )
		var nxt = i->next

		if( i->class = ASTCLASS_TARGETBLOCK ) then
			hSolveOutRedundantNestedTargetblocks( i, i->attrib and ASTATTRIB__ALLTARGET )

			'' Has a parent?
			if( parenttargets <> -1 ) then
				'' Are all the parent's targets covered?
				if( (i->attrib and parenttargets) = parenttargets ) then
					'' Remove this targetblock, preserve only its children
					astReplace( code, i, astBuildGROUPFromChildren( i ) )
				end if
			end if
		else
			hSolveOutRedundantNestedTargetblocks( i, parenttargets )
		end if

		i = nxt
	wend

end sub

''
''     version a
''         <...>
''     version a
''         <...>
''
'' becomes:
''
''     version a
''         <...>
''         <...>
''
private sub hMergeAdjacentVerblocks( byval code as ASTNODE ptr )
	var i = code->head
	while( i )
		var nxt = i->next

		hMergeAdjacentVerblocks( i )

		'' Verblock followed by a 2nd one with the same version(s)?
		if( (i->class = ASTCLASS_VERBLOCK) and (nxt <> NULL) ) then
			if( nxt->class = ASTCLASS_VERBLOCK ) then
				if( astIsEqual( i->expr, nxt->expr ) ) then
					astCloneAppendChildren( i, nxt )
					astRemove( code, nxt )
					'' Re-check this verblock in case there are more following
					nxt = i
				end if
			end if
		end if

		i = nxt
	wend
end sub

'' Same but for targetblocks
private sub hMergeAdjacentTargetblocks( byval code as ASTNODE ptr )
	var i = code->head
	while( i )
		var nxt = i->next

		hMergeAdjacentTargetblocks( i )

		'' Targetblock followed by a 2nd one with the same target(s)?
		if( (i->class = ASTCLASS_TARGETBLOCK) and (nxt <> NULL) ) then
			if( nxt->class = ASTCLASS_TARGETBLOCK ) then
				if( (i->attrib and ASTATTRIB__ALLTARGET) = (nxt->attrib and ASTATTRIB__ALLTARGET) ) then
					astCloneAppendChildren( i, nxt )
					astRemove( code, nxt )
					'' Re-check this targetblock in case there are more following
					nxt = i
				end if
			end if
		end if

		i = nxt
	wend
end sub

private function hBuildIfExprFromVerblockExpr _
	( _
		byval group as ASTNODE ptr, _
		byval versiondefine as zstring ptr _
	) as ASTNODE ptr

	dim as ASTNODE ptr l

	'' a, b, c  ->  (a or b) or c
	assert( group->class = ASTCLASS_GROUP )
	var i = group->head
	do
		'' a  ->  __MYLIB_VERSION__ = a
		var r = astNewBOP( ASTCLASS_EQ, astNewID( versiondefine ), astClone( i ) )

		if( l ) then
			l = astNewBOP( ASTCLASS_OR, l, r )
		else
			l = r
		end if

		i = i->next
	loop while( i )

	function = l
end function

private sub hTurnLastElseIfIntoElse _
	( _
		byval code as ASTNODE ptr, _
		byval i as ASTNODE ptr, _
		byval j as ASTNODE ptr _
	)

	dim as ASTNODE ptr last
	if( j ) then
		last = j->prev
	else
		last = code->tail
	end if

	assert( i <> last )
	assert( last->class = ASTCLASS_PPELSEIF )
	last->class = ASTCLASS_PPELSE
	astDelete( last->expr )
	last->expr = NULL

end sub

''
''     verblock a
''         <...>
''     targetblock win32
''         <...>
''
'' must be ultimately turned into:
''
''     #if __FOO__ = a
''     <...>
''     #endif
''     #ifdef __FB_WIN32__
''     <...>
''     #endif
''
'' As an optimization, we can generate #elseif/#else blocks for adjacent verblocks
'' whose versions aren't overlapping:
''
''     verblock a
''         <...>
''     verblock b
''         <...>
''     verblock c
''         <...>
''
'' becomes:
''
''     #if __FOO__ = a
''     <...>
''     #elseif __FOO__ = b
''     <...>
''     #else
''     <...>
''     #endif
''
private sub hTurnVerblocksIntoPpIfs _
	( _
		byval code as ASTNODE ptr, _
		byval versions as ASTNODE ptr, _
		byval versiondefine as zstring ptr _
	)

	var i = code->head
	while( i )

		'' Process verblocks nested inside structs etc.
		hTurnVerblocksIntoPpIfs( i, versions, versiondefine )

		'' If this is a verblock, try to combine it and following ones
		'' into a "single" #if/#elseif/#endif, as long as they all cover
		'' different versions.
		if( i->class = ASTCLASS_VERBLOCK ) then
			'' Collect a list of all the versions checked for. It's ok to keep
			'' "combining" verblocks into #if/#elseif as long as no duplicates
			'' would have to be added to this list, because an #if/#elseif will
			'' only ever evaluate a single code path.
			var collectedversions = astNewGROUP( astClone( i->expr ) )

			var j = i->next
			while( j )
				if( j->class <> ASTCLASS_VERBLOCK ) then exit while
				if( astGroupContainsAnyChildrenOf( collectedversions, j->expr ) ) then exit while
				astCloneAppend( collectedversions, j->expr )
				j = j->next
			wend

			'' j = last verblock following i that may be combined into an #if/#elseif/#else

			'' The first verblock must become an #if, any following become #elseifs, and the
			'' last can perhaps be an #else. At the end, an #endif must be inserted.
			var k = i
			do
				k->class = iif( k = i, ASTCLASS_PPIF, ASTCLASS_PPELSEIF )
				var newexpr = hBuildIfExprFromVerblockExpr( k->expr, versiondefine )
				astDelete( k->expr )
				k->expr = newexpr

				k = k->next
			loop while( k <> j )

			'' If the collected verblocks cover all versions, then only the first #if check
			'' and any intermediate #elseif checks are needed, but the last check can be turned
			'' into a simple #else.
			if( astGroupsContainEqualChildren( versions, collectedversions ) ) then
				hTurnLastElseIfIntoElse( code, i, j )
			end if

			'' Insert #endif
			astInsert( code, astNew( ASTCLASS_PPENDIF ), j )

			astDelete( collectedversions )
		end if

		i = i->next
	wend
end sub

private sub hOrDefined _
	( _
		byref n as ASTNODE ptr, _
		byref targets as integer, _
		byval target as integer, _
		byval targetdefine as zstring ptr _
	)

	if( targets and target ) then
		targets and= not target

		'' defined( __FB_TARGET__ )
		var def = astNewUOP( ASTCLASS_DEFINED, astNewID( targetdefine ) )

		'' OR with previous expression, if any
		if( n ) then
			n = astNewBOP( ASTCLASS_OR, n, def )
		else
			n = def
		end if
	end if

end sub

'' win32     -> defined( __FB_WIN32__ )
'' dos|linux -> defined( __FB_DOS__ ) or defined( __FB_LINUX__ )
private function hBuildIfExprFromTargets( byval targets as integer ) as ASTNODE ptr
	dim as ASTNODE ptr n
	hOrDefined( n, targets, ASTATTRIB_DOS  , @"__FB_DOS__"   )
	hOrDefined( n, targets, ASTATTRIB_LINUX, @"__FB_LINUX__" )
	hOrDefined( n, targets, ASTATTRIB_WIN32, @"__FB_WIN32__" )
	assert( targets = 0 )
	function = n
end function

'' Same but for targetblocks
private sub hTurnTargetblocksIntoPpIfs( byval code as ASTNODE ptr )
	var i = code->head
	while( i )

		'' Process targetblocks nested inside verblocks/structs etc.
		hTurnTargetblocksIntoPpIfs( i )

		if( i->class = ASTCLASS_TARGETBLOCK ) then
			'' Find end of "mergable" targetblocks...
			var collectedtargets = i->attrib and ASTATTRIB__ALLTARGET
			var j = i->next
			while( j )
				if( j->class <> ASTCLASS_TARGETBLOCK ) then exit while
				if( collectedtargets and (j->attrib and ASTATTRIB__ALLTARGET) ) then exit while
				collectedtargets or= j->attrib and ASTATTRIB__ALLTARGET
				j = j->next
			wend

			'' Turn them into #ifs/#elseifs
			var k = i
			do

				k->class = iif( k = i, ASTCLASS_PPIF, ASTCLASS_PPELSEIF )
				k->expr = hBuildIfExprFromTargets( k->attrib and ASTATTRIB__ALLTARGET )

				k = k->next
			loop while( k <> j )

			'' Perhaps turn the last #elseif into an #else
			if( collectedtargets = ASTATTRIB__ALLTARGET ) then
				hTurnLastElseIfIntoElse( code, i, j )
			end if

			'' Insert #endif
			astInsert( code, astNew( ASTCLASS_PPENDIF ), j )
		end if

		i = i->next
	wend
end sub

'' Add some checks at the top of each of the binding to verify that the
'' version-#define has a valid value.
private sub hAddVersionDefineChecks _
	( _
		byval code as ASTNODE ptr, _
		byval versions as ASTNODE ptr, _
		byval versiondefine as zstring ptr, _
		byval defaultversion as ASTNODE ptr _
	)

	var checks = astNewGROUP( )

	'' If the version #define wasn't #defined, use the default version
	'' (chosen by preset)
	''     #ifndef VER
	''         #define VER default
	''     #endif
	if( defaultversion->class <> ASTCLASS_DUMMYVERSION ) then
		var ppif = astNewPPIF( astNewUOP( ASTCLASS_NOT, astNewUOP( ASTCLASS_DEFINED, astNewID( versiondefine ) ) ) )
		var macro = astNewPPDEFINE( versiondefine )
		macro->expr = astClone( defaultversion )
		astAppend( ppif, macro )
		astAppend( checks, ppif )
		astAppend( checks, astNew( ASTCLASS_PPENDIF ) )
	end if

	'' Complain if the version #define was #defined to an unsupported value
	''     #if VER <> good1 and VER <> good2 and VER <> good3 etc.
	''         #error "VER is #defined to an unsupported value; expected one of: a, b, c"
	''     #endif
	scope
		dim expr as ASTNODE ptr
		dim commalist as string

		scope
			var i = versions->head
			while( i )

				if( i->class <> ASTCLASS_DUMMYVERSION ) then
					'' VER <> version
					var comparison = astNewBOP( ASTCLASS_NE, astNewID( versiondefine ), astClone( i ) )

					dim versionvalue as string
					if( astIsCONSTI( i ) ) then
						versionvalue = str( i->vali )
					else
						assert( i->class = ASTCLASS_STRING )
						versionvalue = """" + *i->text + """"
					end if

					'' AND with previous expression, if any
					if( expr ) then
						expr = astNewBOP( ASTCLASS_AND, expr, comparison )
						commalist += ", " + versionvalue
					else
						expr = comparison
						commalist = versionvalue
					end if
				end if

				i = i->next
			wend
		end scope

		if( expr ) then
			var ppif = astNewPPIF( expr )
			var pperror = astNew( ASTCLASS_PPERROR )
			pperror->expr = astNew( ASTCLASS_STRING, "'" + *versiondefine + "' is #defined to an unsupported value; expected one of: " + commalist )
			astAppend( ppif, pperror )
			astAppend( checks, ppif )
			astAppend( checks, astNew( ASTCLASS_PPENDIF ) )
		end if
	end scope

	astPrepend( code, checks )

end sub

sub astProcessVerblocksAndTargetblocks _
	( _
		byval code as ASTNODE ptr, _
		byval versions as ASTNODE ptr, _
		byval versiondefine as zstring ptr _
	)

	assert( code->class = ASTCLASS_GROUP )

	hSimplifyVerblocks( code, versions )

	hSolveOutRedundantNestedVerblocks( code, NULL )
	hSolveOutRedundantNestedTargetblocks( code, -1 )

	'' It's possible that the above simplification opened up some merging
	'' possibilities between adjacent verblocks.
	hMergeAdjacentVerblocks( code )
	hMergeAdjacentTargetblocks( code )

	hTurnVerblocksIntoPpIfs( code, versions, versiondefine )
	hTurnTargetblocksIntoPpIfs( code )

	hAddVersionDefineChecks( code, versions, versiondefine, versions->head )

end sub
