'' Version expression/block handling functions

#include once "fbfrog.bi"

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

private sub hCombineVersionAndTarget _
	( _
		byval result as ASTNODE ptr, _
		byval targets as integer, _
		byval i as ASTNODE ptr, _
		byval target as integer _
	)

	if( targets and target ) then
		var newi = astClone( i )
		newi->attrib or= target
		astAppend( result, newi )
	end if

end sub

function astCombineVersionsAndTargets _
	( _
		byval versions as ASTNODE ptr, _
		byval targets as integer _
	) as ASTNODE ptr

	var result = astNewGROUP( )

	'' Multiply version(s) with the targets, for example:
	''    versions: 1, 2
	''    targets: linux, win32
	''    combined: 1.linux, 1.win32, 2.linux, 2.win32

	var i = versions->head
	assert( i )
	do

		hCombineVersionAndTarget( result, targets, i, ASTATTRIB_DOS )
		hCombineVersionAndTarget( result, targets, i, ASTATTRIB_LINUX )
		hCombineVersionAndTarget( result, targets, i, ASTATTRIB_WIN32 )

		i = i->next
	loop while( i )

	function = result
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
private sub hRemoveFullTargets( byval group as ASTNODE ptr, byval targets as integer )
	var i = group->head
	while( i )
		if( (i->attrib and ASTATTRIB__ALLTARGET) = targets ) then
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
		byval versions as ASTNODE ptr, _
		byval targets as integer _
	)

	var i = code->head
	while( i )
		var nxt = i->next

		'' Handle nested verblocks inside structs
		hSimplifyVerblocks( i, versions, targets )

		if( i->class = ASTCLASS_VERBLOCK ) then
			hCombineVersionTargets( i->expr )
			hRemoveFullTargets( i->expr, targets )

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
private sub hTurnTargetblocksIntoPpIfs _
	( _
		byval code as ASTNODE ptr, _
		byval targets as integer _
	)

	var i = code->head
	while( i )

		'' Process targetblocks nested inside verblocks/structs etc.
		hTurnTargetblocksIntoPpIfs( i, targets )

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
			if( collectedtargets = targets ) then
				hTurnLastElseIfIntoElse( code, i, j )
			end if

			'' Insert #endif
			astInsert( code, astNew( ASTCLASS_PPENDIF ), j )
		end if

		i = i->next
	wend
end sub

'' Add some checks at the top of each of the binding's files, to verify the
'' version-selection #define and the target system.
private sub hAddVersionDefineChecks _
	( _
		byval code as ASTNODE ptr, _
		byval versions as ASTNODE ptr, _
		byval targets as integer, _
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

	'' Check the target system (some bindings aren't intended to work on DOS, etc.)
	''     #if not defined( __FB_WIN32__ ) and not defined( __FB_LINUX__ )
	''         #error "unsupported target system; this binding supports only: win32, linux"
	''     #endif

	astPrepend( code, checks )

end sub

sub astProcessVerblocksAndTargetblocks _
	( _
		byval code as ASTNODE ptr, _
		byval versions as ASTNODE ptr, _
		byval targets as integer, _
		byval versiondefine as zstring ptr _
	)

	assert( code->class = ASTCLASS_GROUP )

	hSimplifyVerblocks( code, versions, targets )

	hSolveOutRedundantNestedVerblocks( code, NULL )
	hSolveOutRedundantNestedTargetblocks( code, -1 )

	'' It's possible that the above simplification opened up some merging
	'' possibilities between adjacent verblocks.
	hMergeAdjacentVerblocks( code )
	hMergeAdjacentTargetblocks( code )

	hTurnVerblocksIntoPpIfs( code, versions, versiondefine )
	hTurnTargetblocksIntoPpIfs( code, targets )

	hAddVersionDefineChecks( code, versions, targets, versiondefine, versions->head )

end sub
