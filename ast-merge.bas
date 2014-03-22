'' AST merging

#include once "fbfrog.bi"

declare function astMergeVerblocks _
	( _
		byval a as ASTNODE ptr, _
		byval b as ASTNODE ptr _
	) as ASTNODE ptr

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
		byval blast as integer _
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
		              barray, bfirst, blcsfirst - 1 )
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
				if( strStartsWith( *astruct->text, FROG_DUMMYID ) ) then
					assert( *astruct->text <> *bstruct->text )
					assert( strStartsWith( *bstruct->text, FROG_DUMMYID ) )
					assert( *cstruct->text = *astruct->text )

					'' Two structs with dummy ids, being merged together.
					'' hMergeStructsManually() will have re-use a's id as id
					'' for the merged struct, and now we need to manually update
					'' all uses of b's id to now use a's id too.
					'' Or emit a typedef that makes b point to a.
					var typedef = astNew( ASTCLASS_TYPEDEF, bstruct->text )
					astSetType( typedef, TYPE_UDT, astNewID( cstruct->text ) )
					astAppendVerblock( c, astClone( bversion ), NULL, typedef )
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
		hAstMerge( c, aarray, alcslast + 1, alast, barray, blcslast + 1, blast )
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

private function astMergeVerblocks _
	( _
		byval a as ASTNODE ptr, _
		byval b as ASTNODE ptr _
	) as ASTNODE ptr

	var c = astNewGROUP( )
	if( a->class <> ASTCLASS_GROUP ) then a = astNewGROUP( a )
	if( b->class <> ASTCLASS_GROUP ) then b = astNewGROUP( b )

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
	              btable.array, 0, btable.count - 1 )

	decltableEnd( @btable )
	decltableEnd( @atable )

	#if 0
		print "c:"
		astDump( c, 1 )
	#endif

	function = c
end function

function astMergeFiles _
	( _
		byval files1 as ASTNODE ptr, _
		byval files2 as ASTNODE ptr _
	) as ASTNODE ptr

	if( files1 = NULL ) then
		return files2
	end if

	'' Merge files2 into files1
	''
	'' For example:
	'' files1:
	''    libfoo-1/foo.h
	''    libfoo-1/fooconfig.h
	'' files2:
	''    libfoo-2/foo.h
	''    libfoo-2/foo2.h
	'' foo.h exists in both versions and thus the two should be combined,
	'' but the other files only exist in their respective version,
	'' so they shouldn't be combined, just preserved.

	var f2 = files2->head
	while( f2 )

		'' Does a file with similar name exist in files1?
		var name2 = pathStrip( *f2->text )

		var f1 = files1->head
		while( f1 )

			var name1 = pathStrip( *f1->text )
			if( name1 = name2 ) then
				exit while
			end if

			f1 = f1->next
		wend

		if( f1 ) then
			'' File found in files1; merge the two files' ASTs
			if( (f1->expr <> NULL) and (f2->expr <> NULL) ) then
				f1->expr = astMergeVerblocks( f1->expr, f2->expr )
			elseif( f2->expr ) then
				f1->expr = f2->expr
			end if
			f2->expr = NULL
		else
			'' File exists only in files2, copy over to files1
			astCloneAppend( files1, f2 )
		end if

		f2 = f2->next
	wend

	astDelete( files2 )
	function = files1
end function

private sub hFindLcs _
	( _
		byval a as ASTNODE ptr, _
		byref alcsfirst as integer, _
		byref alcslast as integer, _
		byval b as ASTNODE ptr, _
		byref blcsfirst as integer, _
		byref blcslast as integer _
	)

	dim atable as DECLTABLE
	dim btable as DECLTABLE

	decltableInit( @atable, a )
	decltableInit( @btable, b )

	hAstLCS( atable.array, 0, atable.count - 1, alcsfirst, alcslast, _
	         btable.array, 0, btable.count - 1, blcsfirst, blcslast, _
	         ASTEQ_IGNOREHIDDENCALLCONV )

	decltableEnd( @btable )
	decltableEnd( @atable )

end sub

private sub hFindMaxLcs _
	( _
		byval files as ASTNODE ptr, _
		byref maxa as ASTNODE ptr, _
		byref maxalcsfirst as integer, _
		byref maxalcslast as integer, _
		byref maxb as ASTNODE ptr, _
		byref maxblcsfirst as integer, _
		byref maxblcslast as integer _
	)

	maxalcsfirst = 0
	maxblcsfirst = 0
	maxalcslast = -1
	maxblcslast = -1

	var a = files->head
	do
		var b = files->head
		do

			if( a <> b ) then
				dim as integer alcsfirst, alcslast
				dim as integer blcsfirst, blcslast
				hFindLcs( a->expr, alcsfirst, alcslast, _
				          b->expr, blcsfirst, blcslast )

				'' Found LCS?
				if( alcsfirst <= alcslast ) then
					'' Found a pair of files with longer LCS than the previous max?
					if( (maxalcslast - maxalcsfirst + 1) < (alcslast - alcsfirst + 1) ) then
						'' Remember this pair of files as the new max
						maxa = a
						maxalcsfirst = alcsfirst
						maxalcslast  = alcslast
						maxb = b
						maxblcsfirst = blcsfirst
						maxblcslast  = blcslast
					end if
				end if
			end if

			b = b->next
		loop while( b )

		a = a->next
	loop while( a )

end sub

'' Rewrites file content (VERBLOCKs containing declarations), such that the
'' given range of declarations is removed and an #include statement is inserted
'' in their place, referencing the given file name.
''
'' The #include statement is inserted in a VERBLOCK covering all versions, so
'' that it isn't version-specific, because the common header itself contains
'' the common code's VERBLOCKs already (so the version checks are done there,
'' and the #include statement can be generic).
private function hReplaceDeclsWithInclude _
	( _
		byval code as ASTNODE ptr, _
		byval table as DECLTABLE ptr, _
		byval first as integer, _
		byval last as integer, _
		byval versions as ASTNODE ptr, _
		byval includefile as zstring ptr _
	) as ASTNODE ptr

	var result = astNewGROUP( )

	for i as integer = 0 to table->count - 1
		'' First common declaration? Add #include instead of preserving it
		if( i = first ) then
			var inc = astNew( ASTCLASS_PPINCLUDE, includefile )
			inc->attrib or= ASTATTRIB_UNIQUE
			astAppendVerblock( result, astClone( versions ), NULL, _
					inc )
		'' Preserve other declarations as long as they're not part of the common ones
		elseif( (i < first) or (i > last) ) then
			astAppendVerblock( result, astClone( table->array[i].v ), NULL, _
					astClone( table->array[i].n ) )
		end if
	next

	assert( result->head )  '' should at least contain the #include

	function = result
end function

private function hMakeUniqueNameForExtractedHeader( ) as zstring ptr
	static s as zstring * 32
	static count as integer
	s = "fbfrog-common-" & count & ".h"
	function = @s
	count += 1
end function

private sub hExtractCommonCode _
	( _
		byval files as ASTNODE ptr, _
		byval versions as ASTNODE ptr, _
		byref a as ASTNODE ptr, _
		byref alcsfirst as integer, _
		byref alcslast as integer, _
		byref b as ASTNODE ptr, _
		byref blcsfirst as integer, _
		byref blcslast as integer _
	)

	dim atable as DECLTABLE
	dim btable as DECLTABLE

	decltableInit( @atable, a->expr )
	decltableInit( @btable, b->expr )

	assert( (alcsfirst <= alcslast) and (alcslast < atable.count) )
	assert( (blcsfirst <= blcslast) and (blcslast < btable.count) )

	'' If the LCS covers all of a or b, then we can use that as the common #include,
	'' instead of adding a new file.
	dim as zstring ptr commonname = any
	var rewrite_a = TRUE, rewrite_b = TRUE
	if( (alcslast - alcsfirst + 1) = atable.count ) then
		commonname = a->text
		rewrite_a = FALSE
	elseif( (blcslast - blcsfirst + 1) = btable.count ) then
		commonname = b->text
		rewrite_b = FALSE
	else
		'' Create a new file out of those common decls
		commonname = hMakeUniqueNameForExtractedHeader( )

		'' Copy the common decls into a new GROUP, preserving their VERBLOCKs
		var commondecls = astNewGROUP( )
		for i as integer = 0 to (alcslast - alcsfirst + 1) - 1
			astAppendVerblock( commondecls, _
					astClone( atable.array[alcsfirst+i].v ), _
					astClone( btable.array[blcsfirst+i].v ), _
					astClone( atable.array[alcsfirst+i].n ) )
		next

		'' Add the new file
		var commonfile = astNewTEXT( commonname )
		commonfile->expr = commondecls
		astAppend( files, commonfile )
	end if

	'' Rewrite a and b, removing the common decls, and inserting #includes
	if( rewrite_a ) then a->expr = hReplaceDeclsWithInclude( a->expr, @atable, alcsfirst, alcslast, versions, commonname )
	if( rewrite_b ) then b->expr = hReplaceDeclsWithInclude( b->expr, @btable, blcsfirst, blcslast, versions, commonname )

	decltableEnd( @btable )
	decltableEnd( @atable )

end sub

''
'' Given a list of root headers each containing all declarations that were
'' reachable through #includes, it's possible that they contain some common
'' code due to a common #include. This duplication of code should be reverted
'' by finding such common sequences of declarations and extracting them into
'' common #includes again.
''
'' For example, 3 files called a, b, c:
''	a	b	c
''	1	1	2
''	2	2	4
''	3	4	5
'' Some have some declarations in common, e.g. a/1,2 and b/1,2.
''
'' Simple solution, 1st common declarations found, extracted into new header d:
''	a	b	c	d
''	<d>	<d>	2	1
''	2	2	4
''	3	4	5
''
''	a	b	c	d	e
''	<d>	<d>	2	1	2
''	<e>	<e>	4
''	3	4	5
''
''	a	b	c	d	e	f
''	<d>	<d>	<f>	1	<f>	2
''	<e>	<e>	4
''	3	4	5
''
''	a	b	c	d	f
''	<d>	<d>	<f>	1	2
''	<f>	<f>	4
''	3	4	5
''
''	a	b	c	d	f	g
''	<d>	<d>	<f>	1	2	4
''	<f>	<f>	<g>
''	3	<g>	5
''
'' Better solution, where the longest common substring is extracted first:
''	a	b	c	d
''	<d>	<d>	2	1
''			4	2
''	3	4	5
''
''	a	b	c	d	e
''	<d>	<d>	<e>	1	2
''			4	<e>
''	3	4	5
''
''	a	b	c	d	e	f
''	<d>	<d>	<e>	1	2	4
''			<f>	<e>
''	3	<f>	5
''
sub astExtractCommonCodeFromFiles _
	( _
		byval files as ASTNODE ptr, _
		byval versions as ASTNODE ptr _
	)

	do
		dim as ASTNODE ptr a, b
		dim as integer alcsfirst, alcslast, blcsfirst, blcslast

		hFindMaxLcs( files, a, alcsfirst, alcslast, b, blcsfirst, blcslast )

		'' No LCS found?
		if( a = NULL ) then
			exit do
		end if

		print (alcslast - alcsfirst + 1) & " common declarations in " + *a->text + " and " + *b->text

		hExtractCommonCode( files, versions, _
			a, alcsfirst, alcslast, b, blcsfirst, blcslast )
	loop

end sub
