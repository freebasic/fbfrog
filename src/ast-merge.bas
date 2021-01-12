''
'' AST merging based on longest-common-substring algorithm (diff'ing),
''    which results in individual sections of the AST being wrapped in VERBLOCK
''    nodes holding the conditions (e.g. #ifdef __FB_WIN32__) for that block of
''    code.
''
'' #if generation,
''    which (after all merging is done) converts the VERBLOCKs into #ifs and
''    tries to reduce the #if condition expressions as much as possible to
''    eliminate duplicate checks. For example, for a VERBLOCK that covers all
''    possible APIs, we don't need to generate any #if at all since its
''    condition would always be true. And in case we have a check like this:
''        #if (__FB_WIN32__ and __FB_64BIT__) or (__FB_LINUX__ and __FB_64BIT__)
''    that can be simplified to:
''        #if __FB_64BIT__ and (__FB_WIN32__ or __FB_LINUX__)
''    etc.
''

#include once "ast-merge.bi"

#include once "fbfrog.bi"
#include once "util-hash.bi"
#include once "util.bi"

''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''

type DECLNODE
	n	as AstNode ptr  '' The declaration at that index
	hash	as ulong        '' Precalculated hash value for the declaration
	apis	as ApiBits      '' APIs bitmask of the declaration's parent VERBLOCK
end type

type DeclTable
	array	as DECLNODE ptr
	count	as integer
	room	as integer
	declare static function calcHash(byval n as AstNode ptr) as ulong
	declare sub add(byval n as AstNode ptr, byval apis as ApiBits)
	declare sub addAll(byval code as AstNode ptr)
	declare destructor()
end type

'' AstNode kind and identifier are the 2 main points to quickly distinguish two declarations.
'' Care must be taken though; callconv/ASTATTRIB_HIDECALLCONV flags shouldn't be calculated into
'' the hash though because hAstLCS() and hFindCommonCallConvsOnMergedDecl() do custom handling for them...
function DeclTable.calcHash(byval n as AstNode ptr) as ulong
	dim as ulong hash = n->kind
	if n->text then
		hash or= hashHash(n->text) shl 8
	end if
	function = hash
end function

sub DeclTable.add(byval n as AstNode ptr, byval apis as ApiBits)
	if count = room then
		room += 256
		array = reallocate(array, room * sizeof(DECLNODE))
	end if
	with array[count]
		.n = n
		.hash = calcHash(n)
		.apis = apis
	end with
	count += 1
end sub

sub DeclTable.addAll(byval code as AstNode ptr)
	'' Add each declaration node from the AST to the table
	'' For each VERBLOCK...
	assert(code->kind = ASTKIND_GROUP)
	var verblock = code->head
	while verblock
		assert(astIsVERBLOCK(verblock))

		'' For each declaration in that VERBLOCK...
		var decl = verblock->head
		while decl
			add(decl, verblock->apis)
			decl = decl->nxt
		wend

		verblock = verblock->nxt
	wend
end sub

destructor DeclTable()
	deallocate(array)
end destructor

''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''

function astDumpPrettyVersion(byval n as AstNode ptr) as string
	dim s as string

	select case n->kind
	case ASTKIND_VERAND
		var i = n->head
		while i
			s += astDumpPrettyVersion(i)
			i = i->nxt
			if i then
				s += "."
			end if
		wend

	case ASTKIND_VERNUMCHECK
		s = frog.versiondefine + "=" + frog.vernums(n->vernum)

	case ASTKIND_DEFINED
		s = *n->text

	case ASTKIND_NOT
		s = "(not " + astDumpPrettyVersion(n->head) + ")"

	case else
		assert(FALSE)
	end select

	function = s
end function

private function astNewGroupLike _
	( _
		byval astkind as integer, _
		byval a as AstNode ptr, _
		byval b as AstNode ptr _
	) as AstNode ptr

	if a andalso (a->kind = astkind) then a->kind = ASTKIND_GROUP
	if b andalso (b->kind = astkind) then b->kind = ASTKIND_GROUP

	function = astNew(astkind, astNewGROUP(a, b))
end function

function astNewVERAND(byval a as AstNode ptr, byval b as AstNode ptr) as AstNode ptr
	function = astNewGroupLike(ASTKIND_VERAND, a, b)
end function

function astNewVEROR(byval a as AstNode ptr, byval b as AstNode ptr) as AstNode ptr
	function = astNewGroupLike(ASTKIND_VEROR, a, b)
end function

function astNewVERNUMCHECK(byval vernum as integer) as AstNode ptr
	var n = astNew(ASTKIND_VERNUMCHECK)
	n->vernum = vernum
	function = n
end function

private function astNewVERBLOCK(byval apis as ApiBits, byval children as AstNode ptr) as AstNode ptr
	var n = astNew(ASTKIND_VERBLOCK, children)
	n->apis = apis
	function = n
end function

'' Just as the whole file AST is wrapped in a verblock, the fields of struct
'' should be aswell, so hMergeStructsManually() doesn't have to handle both
'' cases of "fresh still unwrapped fields" and "already wrapped from previous
'' merge", but only the latter.
private sub hWrapStructFieldsInVerblocks(byval api as ApiBits, byval code as AstNode ptr)
	var i = code->head
	while i
		hWrapStructFieldsInVerblocks(api, i)
		i = i->nxt
	wend

	if code->isMergableBlock() then
		var newfields = astNewVERBLOCK(api, code->cloneChildren())
		code->removeChildren()
		code->append(newfields)
	end if
end sub

private function astWrapFileInVerblock(byval api as ApiBits, byval code as AstNode ptr) as AstNode ptr
	hWrapStructFieldsInVerblocks(api, code)
	function = astNewVERBLOCK(api, code)
end function

private sub hVerblockAppend _
	( _
		byval n as AstNode ptr, _
		byval apis as ApiBits, _
		byval child as AstNode ptr _
	)

	'' If the tree's last VERBLOCK covers the same versions, then just add
	'' the new children nodes to that instead of opening a new VERBLOCK.
	var verblock = n->tail
	if verblock andalso astIsVERBLOCK(verblock) then
		if verblock->apis.equals(apis) then
			verblock->append(child)
			exit sub
		end if
	end if

	n->append(astNewVERBLOCK(apis, child))
end sub

private sub hAddDecl(byval c as AstNode ptr, byval array as DECLNODE ptr, byval i as integer)
	hVerblockAppend(c, array[i].apis, array[i].n->clone())
end sub

''
'' Merge two procdecls which differ only in their callconv and
'' ASTATTRIB_HIDECALLCONV flags. This will only be reached if astIsEqual()
'' returned TRUE, but in hAstLCS()'s astIsEqual()
''      wouldn't have treated them as equal.
'' See also hTurnCallConvIntoExternBlock():
'' ASTATTRIB_HIDECALLCONV means that the callconv is covered by the surrounding
'' Extern block, in which case the callconv does not need to be emitted
'' explicitly.
''
'' Cases that reach this function:
''
'' a) Different callconv, but both have ASTATTRIB_HIDECALLCONV.
''    * can be emitted without explicit callconv, as the Extern blocks will
''      take care of that and remap the callconv as needed.
''    * The merged node shouldn't have any callconv flag at all,
''      but only ASTATTRIB_HIDECALLCONV.
''
'' b) Same callconv, but only one side has ASTATTRIB_HIDECALLCONV.
''    * callconv must be preserved on the merged node, so it will be emitted
''      explicitly, since the Extern blocks only cover it on one side, not both.
''    * ASTATTRIB_HIDECALLCONV shouldn't be preserved in this case.
''
'' c) Same callconv and ASTATTRIB_HIDECALLCONV on both sides
''    * trivial to merge - both callconv and ASTATTRIB_HIDECALLCONV should be
''      preserved.
''
'' The same applies to procptr subtypes, and to be able to handle them, this
'' function is recursive.
''
private sub hFindCommonCallConvsOnMergedDecl _
	( _
		byval mdecl as AstNode ptr, _
		byval adecl as AstNode ptr, _
		byval bdecl as AstNode ptr _
	)

	assert(mdecl->kind = adecl->kind)
	assert(adecl->kind = bdecl->kind)

	if mdecl->kind = ASTKIND_PROC then

		var ahide = ((adecl->attrib and ASTATTRIB_HIDECALLCONV) <> 0)
		var bhide = ((bdecl->attrib and ASTATTRIB_HIDECALLCONV) <> 0)
		var acallconv = (adecl->attrib and (ASTATTRIB_CDECL or ASTATTRIB_STDCALL))
		var bcallconv = (bdecl->attrib and (ASTATTRIB_CDECL or ASTATTRIB_STDCALL))
		#if __FB_DEBUG__
			var mhide = ((mdecl->attrib and ASTATTRIB_HIDECALLCONV) <> 0)
			var mcallconv = (mdecl->attrib and (ASTATTRIB_CDECL or ASTATTRIB_STDCALL))
		#endif

		if ahide and bhide then
			'' ASTATTRIB_HIDECALLCONV on both sides.
			if acallconv = bcallconv then
				'' Same callconv, trivial merge.
				assert(mcallconv = acallconv)
				assert(mcallconv = bcallconv)
			else
				'' Different callconv; but at least both sides have ASTATTRIB_HIDECALLCONV,
				'' so we can forget about the callconvs and let the EXTERN blocks cover it.
				mdecl->attrib and= not (ASTATTRIB_CDECL or ASTATTRIB_STDCALL)
			end if
			assert(mhide) '' was preserved by astClone() already
		else
			assert(acallconv = bcallconv)

			if ahide or bhide then
				'' Same callconv, but ASTATTRIB_HIDECALLCONV only on one side.
				'' We can merge them, but the callconv can't be hidden.
				mdecl->attrib and= not ASTATTRIB_HIDECALLCONV
				assert(mcallconv <> 0) '' ditto
			''else
				'' Same callconv and no ASTATTRIB_HIDECALLCONV, trivial merge.
			end if
		end if
	end if

	'' Don't forget the procptr subtypes
	if typeGetDt(mdecl->dtype) = TYPE_PROC then
		assert(typeGetDt(adecl->dtype) = TYPE_PROC)
		assert(typeGetDt(bdecl->dtype) = TYPE_PROC)
		hFindCommonCallConvsOnMergedDecl(mdecl->subtype, adecl->subtype, bdecl->subtype)
	end if

	var mchild = mdecl->head
	var achild = adecl->head
	var bchild = bdecl->head
	while mchild
		assert(achild)
		assert(bchild)

		hFindCommonCallConvsOnMergedDecl(mchild, achild, bchild)

		mchild = mchild->nxt
		achild = achild->nxt
		bchild = bchild->nxt
	wend
	assert(mchild = NULL)
	assert(achild = NULL)
	assert(bchild = NULL)
end sub

private sub hAddMergedDecl _
	( _
		byval c as AstNode ptr, _
		byval aarray as DECLNODE ptr, _
		byval ai as integer, _
		byval barray as DECLNODE ptr, _
		byval bi as integer _
	)

	var adecl = aarray[ai].n
	var bdecl = barray[bi].n
	var aapis = aarray[ai].apis
	var bapis = barray[bi].apis

	assert(adecl->kind = bdecl->kind)

	''
	'' The LCS may include merged blocks (structs/unions/enums/renamelists) that were put
	'' into the LCS despite having different children (fields/enumconsts/etc.) on both sides.
	''
	'' They should be merged recursively now, so the block itself can be common,
	'' while the children may be version dependant.
	''
	'' (This relies on blocks to be allowed to match in the hAstLCS() call,
	'' even if they have different children)
	''
	dim mdecl as AstNode ptr
	if adecl->isMergableBlock() then

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

		var achildren = adecl->cloneChildren()
		var bchildren = bdecl->cloneChildren()

		'' Merge both set of children
		var mchildren = astMergeVerblocks(achildren, bchildren)

		'' Create a result block with the new set of children
		mdecl = adecl->cloneNode()
		mdecl->append(mchildren)
	else
		'' "Merge" a and b by cloning a. They've compared equal in astIsEqual() so this works.
		'' Below we only need to cover a few additional cases where astIsEqual() is more permissive
		'' than a true equality check: it allows merging of a/b even if they're slightly different.
		'' This currently affects the calling convention only. In such cases, just cloning a isn't
		'' enough and some actual merging work is needed.
		mdecl = adecl->clone()

		hFindCommonCallConvsOnMergedDecl(mdecl, adecl, bdecl)
	end if

	'' Add struct to result tree, under both a's and b's version numbers
	dim mapis as ApiBits
	mapis.set(aapis)
	mapis.set(bapis)
	hVerblockAppend(c, mapis, mdecl)

end sub

''
'' Determine the longest common substring, by building an l x r matrix:
''
'' if l[i] = r[j] then
''     if i-1 or j-1 would be out-of-bounds then
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
'' To avoid a huge memory allocation, we don't allocate the whole matrix
'' but only 2 rows, which is really all the LCS algorithm needs.
''
'' For example, given 2 ASTs with 10k declarations each (that can happen
'' easily with huge libraries), allocating the whole matrix would mean:
''    4 bytes * 10k * 10k = 381 MiB
'' while with 2 rows it's only:
''    4 bytes * 10k * 2 = 78 KiB
''
'' And using 4 bytes to hold each length is overkill too - it's surely enough
'' to use 2 bytes and put a limit of max. 65535 declarations per API. Even the
'' whole GTK+/GLib/ATK/Cairo/Pango/etc API results in only ~20k declarations...
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
		byref rlcslast as integer _
	)

	var llen = llast - lfirst + 1
	var rlen = rlast - rfirst + 1

	const MAXROWLEN = &hFFFF

	'' Currently using USHORTs to store the lengths of common sequences of
	'' declarations. It should be enough...
	if (llen > MAXROWLEN) or (rlen > MAXROWLEN) then
		oops("hAstLCS(): soft-limited to " & MAXROWLEN & " declarations per " + _
			"API, but here we have " & llen & " and " & rlen)
	end if

	static row1(0 to MAXROWLEN-1) as ushort
	static row2(0 to MAXROWLEN-1) as ushort

	dim as integer maxlen, maxlenl, maxlenr

	'' previousrow = row for l-1 (NULL for l = 0)
	''  currentrow = row for l
	dim as ushort ptr  currentrow = @row1(0)
	dim as ushort ptr previousrow = @row2(0)

	for l as integer = 0 to llen-1
		for r as integer = 0 to rlen-1
			var length = 0

			var ldecl = @larray[lfirst+l]
			var rdecl = @rarray[rfirst+r]

			if ldecl->hash = rdecl->hash then
				if astIsEqual(ldecl->n, rdecl->n, TRUE) then
					if (l = 0) or (r = 0) then
						length = 1
					else
						length = previousrow[r-1] + 1
					end if
				end if
			end if

			if maxlen < length then
				maxlen = length
				maxlenl = l
				maxlenr = r
			end if

			currentrow[r] = length
		next
		swap currentrow, previousrow
	next

	llcslast = lfirst + maxlenl
	rlcslast = rfirst + maxlenr
	llcsfirst = llcslast - maxlen + 1
	rlcsfirst = rlcslast - maxlen + 1
end sub

private sub hAstMerge _
	( _
		byval c as AstNode ptr, _
		byval aarray as DECLNODE ptr, _
		byval afirst as integer, _
		byval alast as integer, _
		byval barray as DECLNODE ptr, _
		byval bfirst as integer, _
		byval blast as integer _
	)

	'' No longest common substring possible?
	if afirst > alast then
		'' Add bfirst..blast to result
		for i as integer = bfirst to blast
			hAddDecl(c, barray, i)
		next
		exit sub
	elseif bfirst > blast then
		'' Add afirst..alast to result
		for i as integer = afirst to alast
			hAddDecl(c, aarray, i)
		next
		exit sub
	end if

	'' Find longest common substring
	dim as integer alcsfirst, alcslast, blcsfirst, blcslast
	hAstLCS(aarray, afirst, alast, alcsfirst, alcslast, _
	         barray, bfirst, blast, blcsfirst, blcslast)

	'' No LCS found?
	if alcsfirst > alcslast then
		'' Add a first, then b. This order makes the most sense: keeping
		'' the old declarations at the top, add new ones to the bottom.
		for i as integer = afirst to alast
			hAddDecl(c, aarray, i)
		next
		for i as integer = bfirst to blast
			hAddDecl(c, barray, i)
		next
		exit sub
	end if

	'' Do both sides have decls before the LCS?
	if (alcsfirst > afirst) and (blcsfirst > bfirst) then
		'' Do LCS on that recursively
		hAstMerge(c, aarray, afirst, alcsfirst - 1, _
		              barray, bfirst, blcsfirst - 1)
	elseif alcsfirst > afirst then
		'' Only a has decls before the LCS; copy them into result first
		for i as integer = afirst to alcsfirst - 1
			hAddDecl(c, aarray, i)
		next
	elseif blcsfirst > bfirst then
		'' Only b has decls before the LCS; copy them into result first
		for i as integer = bfirst to blcsfirst - 1
			hAddDecl(c, barray, i)
		next
	end if

	'' Add LCS
	assert((alcslast - alcsfirst + 1) = (blcslast - blcsfirst + 1))
	for i as integer = 0 to (alcslast - alcsfirst + 1)-1
		hAddMergedDecl(c, aarray, alcsfirst + i, barray, blcsfirst + i)
	next

	'' Do both sides have decls behind the LCS?
	if (alcslast < alast) and (blcslast < blast) then
		'' Do LCS on that recursively
		hAstMerge(c, aarray, alcslast + 1, alast, barray, blcslast + 1, blast)
	elseif alcslast < alast then
		'' Only a has decls behind the LCS
		for i as integer = alcslast + 1 to alast
			hAddDecl(c, aarray, i)
		next
	elseif blcslast < blast then
		'' Only b has decls behind the LCS
		for i as integer = blcslast + 1 to blast
			hAddDecl(c, barray, i)
		next
	end if

end sub

function astMergeVerblocks _
	( _
		byval a as AstNode ptr, _
		byval b as AstNode ptr _
	) as AstNode ptr

	var c = astNewGROUP()
	a = astNewGROUP(a)
	b = astNewGROUP(b)

	'' Create a lookup table for each side, so the LCS algorithm can do
	'' index-based lookups in O(1) instead of having to cycle through the
	'' whole list of preceding nodes everytime which was terribly slow.
	dim atable as DeclTable
	dim btable as DeclTable

	atable.addAll(a)
	btable.addAll(b)

	''
	'' DeclTable precalculates hashes for A's and B's declarations.
	'' hAstLCS() can then quickly detect declarations that are different and
	'' avoid the slow astIsEqual() in such cases.
	''
	'' Using the hashes is worth it, because the LCS algorithm checks all
	'' the combinations of A's and B's declarations, i.e. an AxB matrix.
	'' Even if A and B are completely equal (and AxB is an identity matrix)
	'' there will be at most min(len(A), len(B)) true comparisons that need
	'' to be verified via astIsEqual(), while the vast majority of false
	'' comparisons can be quickly ignored due to the hash mismatch.
	''
	'' Precalculating astIsEqual() results for every entry of the AxB matrix
	'' would probably not be worth it though, because it does not reduce the
	'' amount of astIsEqual() calls.
	''
	'' The only case where it could be an advantage to cache the
	'' astIsEqual() results would be if A and B differ a lot and the LCS
	'' algorithm ends up being run multiple times recursively to merge the
	'' sections above/below an LCS. In that case the same declarations will
	'' be compared multiple times and the caching would speed that up.
	''
	'' However, in practice A and B are usually very similar/almost equal,
	'' thus the LCS will typically be very big, and there won't be many
	'' subsections to merge recursively... and it would have a noticable
	'' memory cost even if compressed into bits: Assuming 20k x 20k
	'' declarations, that would be 400 million bitflags, ~48 MiB.
	''

	hAstMerge(c, atable.array, 0, atable.count - 1, _
	             btable.array, 0, btable.count - 1)

	delete a
	delete b

	function = c
end function

sub astMergeNext(byval api as ApiBits, byref final as AstNode ptr, byref incoming as AstNode ptr)
	incoming = astWrapFileInVerblock(api, incoming)
	if final = NULL then
		final = astNewGROUP(incoming)
	else
		final = astMergeVerblocks(final, incoming)
	end if
	incoming = NULL
end sub

''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''

''
'' Structs/unions/enums can have nested verblocks (to allow fields to be
'' versioned). If a nested verblock covers >= of the parent verblock's versions,
'' then its checking expression would always evaluate to true, so it's useless
'' and can be removed.
''
'' At the very least,
''
''     verblock <foo>
''         struct UDT
''             verblock <foo>
''                 field x as integer
''
'' can be simplified to:
''
''     verblock <foo>
''         struct UDT
''             field x as integer
''
'' Similar to that, verblocks at the toplevel can be solved out, if they cover
'' all possible versions. (think of them as being nested in a global verblock)
''
private sub hSolveOutRedundantVerblocks(byval code as AstNode ptr, byval parentapis as ApiBits)
	var i = code->head
	while i
		var nxt = i->nxt

		if i->kind = ASTKIND_VERBLOCK then
			hSolveOutRedundantVerblocks(i, i->apis)

			assert(parentapis.hasAtLeast1Set())
			'' Nested verblock covers at least the parent's versions?
			if i->apis.coversAtLeast(parentapis) then
				'' Remove this verblock, preserve only its children
				code->replace(i, i->cloneChildren())
			end if
		else
			hSolveOutRedundantVerblocks(i, parentapis)
		end if

		i = nxt
	wend
end sub

#define hCalcIfConditionWeight(expr) ((expr)->count())

'' If the #if block's condition expression is bigger than that of
'' the #else block, swap the two blocks.
''
'' This way we can make the #if block easier to read by hiding the
'' more complex conditions of the two:
''     #if dos or linux or freebsd or darwin
''         [dos/unix]
''     #else
''         [windows]
''     #endif
'' =>
''     #if windows
''         [windows]
''     #else
''         [dos/unix]
''     #endif
private sub maybeSwapIfElseWithoutRelinking(byval ifblock as AstNode ptr)
	var elseblock = ifblock->nxt
	assert(astIsPPIF(ifblock) and astIsPPELSE(elseblock))
	if hCalcIfConditionWeight(ifblock->expr) > hCalcIfConditionWeight(elseblock->expr) then
		swap ifblock->expr, elseblock->expr
		swap ifblock->apis, elseblock->apis
		swap ifblock->head, elseblock->head
		swap ifblock->tail, elseblock->tail
	end if
end sub

''
'' verblocks must be turned into #if/#endif blocks. Doing that on the AST level
'' instead of when emitting allows us to generate #elseifs/#elses for adjacent
'' verblocks as long as they cover different versions.
''
'' For example: (VER=1, VER=2, VER=3).(defined(FOO), (not defined(FOO)))
''
''     verblock VER=1
''         <...>
''     verblock VER=2
''         <...>
''     <...>
''     verblock defined(FOO)
''         <...>
''     verblock (not defined(FOO))
''         <...>
''
'' becomes:
''
''     #if VER=1
''         <...>
''     #elseif VER=2
''         <...>
''     #endif
''     <...>
''     #ifdef FOO
''         <...>
''     #else
''         <...>
''     #endif
''
private sub hTurnVerblocksIntoPpIfs(byval code as AstNode ptr)
	'' Process all nested verblocks first
	'' (we may reorder verblocks below when turning them into #ifs, and
	'' then it's too easy to forget nested ones)
	var i = code->head
	while i
		hTurnVerblocksIntoPpIfs(i)
		i = i->nxt
	wend

	'' Turn verblocks into #ifs (at this recursion level only)
	i = code->head
	while i

		if astIsVERBLOCK(i) then
			'' Turn the 1st verblock into an #if
			i->kind = ASTKIND_PPIF

			'' Find all VERBLOCKs in a row, if any, as long as they cover different versions
			'' (as long as no duplicates would be added to the list of collected versions)
			'' and turn them into #elseif's while at it.
			var j = i->nxt
			var collected = i->apis
			while j andalso astIsVERBLOCK(j) andalso collected.containsNoneOf(j->apis)
				j->kind = ASTKIND_PPELSEIF
				collected.set(j->apis)
				j = j->nxt
			wend

			'' j = node behind the last VERBLOCK in the row, or NULL if at EOF

			'' If the collected verblocks cover all versions, then only the first #if check
			'' and any intermediate #elseif checks are needed, but the last check can be turned
			'' into a simple #else.
			if collected.equals(frog.fullapis) then
				var last = iif(j, j->prev, code->tail)
				'' But only if we've got more than 1 verblock
				if i <> last then
					assert(last->kind = ASTKIND_PPELSEIF)
					last->kind = ASTKIND_PPELSE

					'' Only 2 verblocks? (i.e. just an #if and an #else)
					if i->nxt = last then
						maybeSwapIfElseWithoutRelinking(i)
					end if
				end if
			end if

			'' Insert #endif
			code->insert(astNew(ASTKIND_PPENDIF), j)
		end if

		i = i->nxt
	wend
end sub

type CONDINFO
	cond		as AstNode ptr	'' The condition expression
	count		as integer	'' How often it was seen
end type

type CondCounter
	conds as CONDINFO ptr
	condcount as integer
	declare destructor()
	declare function find(byval cond as AstNode ptr) as integer
	declare sub add(byval cond as AstNode ptr)
	declare sub count(byval cond as AstNode ptr)
	declare sub findMaxCount(byref maxcount as integer, byref imaxcount as integer)
	declare function findMostCommon() as AstNode ptr
end type

destructor CondCounter()
	deallocate(conds)
end destructor

function CondCounter.find(byval cond as AstNode ptr) as integer
	for i as integer = 0 to condcount - 1
		if astIsEqual(conds[i].cond, cond, FALSE) then
			return i
		end if
	next
	return -1
end function

sub CondCounter.add(byval cond as AstNode ptr)
	var i = condcount
	condcount += 1
	conds = reallocate(conds, condcount * sizeof(CONDINFO))
	conds[i].cond = cond
	conds[i].count = 1
end sub

sub CondCounter.count(byval cond as AstNode ptr)
	var i = find(cond)
	if i >= 0 then
		conds[i].count += 1
	else
		add(cond)
	end if
end sub

sub CondCounter.findMaxCount(byref maxcount as integer, byref imaxcount as integer)
	maxcount = 0
	imaxcount = -1
	for i as integer = 0 to condcount - 1
		if maxcount < conds[i].count then
			maxcount = conds[i].count
			imaxcount = i
		end if
	next
end sub

function CondCounter.findMostCommon() as AstNode ptr
	if condcount = 0 then
		return NULL
	end if

	dim as integer maxcount, imaxcount
	findMaxCount(maxcount, imaxcount)
	assert((imaxcount >= 0) and (imaxcount < condcount))

	'' No point extracting a condition that only appeared once
	if conds[imaxcount].count < 2 then
		return NULL
	end if

	return conds[imaxcount].cond
end function

private function hDetermineMostCommonCondition(byval veror as AstNode ptr) as AstNode ptr
	assert(astIsVEROR(veror))

	'' Build list of all conditions and count them. The one with the max
	'' count is the most common.
	dim counter as CondCounter

	var verand = veror->head
	while verand
		if astIsVERAND(verand) then
			var cond = verand->head
			while cond
				counter.count(cond)
				cond = cond->nxt
			wend
		end if
		verand = verand->nxt
	wend

	return counter.findMostCommon()
end function

private function hIsCondition(byval n as AstNode ptr) as integer
	function = (not astIsVEROR(n)) and (not astIsVERAND(n))
end function

'' Determine which OS's are covered by a VEROR
type OSDefineChecker
	covered(0 to OS__COUNT-1) as byte
	declare sub check(byval veror as AstNode ptr)
end type

sub OSDefineChecker.check(byval veror as AstNode ptr)
	assert(astIsVEROR(veror))
	var i = veror->head
	while i
		if astIsDEFINED(i) then
			for os as integer = 0 to OS__COUNT - 1
				if *i->text = *osinfo(os).fbdefine then
					covered(os) = TRUE
				end if
			next
		end if
		i = i->nxt
	wend
end sub

private function coversAllUnix(byval veror as AstNode ptr) as integer
	dim checker as OSDefineChecker
	checker.check(veror)
	for os as integer = 0 to OS__COUNT - 1
		if osinfo(os).is_unix and (not checker.covered(os)) then
			return FALSE
		end if
	next
	return TRUE
end function

private sub replaceUnixChecks(byval veror as AstNode ptr)
	assert(astIsVEROR(veror))
	var i = veror->head
	while i
		var nxt = i->nxt
		if astIsDEFINED(i) then
			for os as integer = 0 to OS__COUNT - 1
				if osinfo(os).is_unix andalso (*i->text = *osinfo(os).fbdefine) then
					veror->remove(i)
					exit for
				end if
			next
		end if
		i = nxt
	wend
	veror->append(astNewDEFINED("__FB_UNIX__"))
end sub

private function hSimplify(byval n as AstNode ptr, byref changed as integer) as AstNode ptr
	if n = NULL then return NULL
	if hIsCondition(n) then return n

	scope
		var i = n->head
		while i
			var newi = hSimplify(i->clone(), changed)

			'' Don't add a VEROR as a child of a VEROR; instead, add
			'' the VEROR's chidlren to the VEROR's parent. (same for VERANDs)
			if newi andalso (newi->kind = n->kind) then
				newi->kind = ASTKIND_GROUP
			end if

			i = n->replace(i, newi)
		wend
	end scope

	'' Single child, or none at all? Solve out the VEROR/VERAND.
	if n->head = n->tail then
		changed = TRUE
		dim child as AstNode ptr = NULL
		if n->head then
			child = n->head->clone()
		end if
		delete n
		return child
	end if

	if astIsVEROR(n) = FALSE then
		return n
	end if

	'' Solve out "complete" VERORs - VERORs that cover all possible choices
	if frog.completeverors->groupContains(n) then
		changed = TRUE
		delete n
		return NULL
	end if

	''
	'' If two VERANDs in a VEROR have a common condition, they can be
	'' simplified:
	''    a.b, a.c        =  a.(b, c)
	''    x, a.b, a.c, y  =  x, a.(b, c), y
	'' and this applies not only to common prefixes/suffixes, but to
	'' just any common conditions:
	''    1.a.x, 2.a.y  =  a.(1.x, 2.y)
	'' The order of operands really doesn't matter:
	''    a.(b, c)
	'' is the same as:
	''    (b, c).a
	''
	'' Sometimes there are multiple solutions:
	''    a.1, a.2, b.2    =    a.(1, 2), b.2    vs.    a.1, 2.(a, b)
	''
	'' We should simplify as much as possible, and we should simplify the
	'' most common conditions first, because that's what you'd do if doing
	'' it manually. I.e., it produced the best and most expected results.
	''

	var mostcommoncond = hDetermineMostCommonCondition(n)
	if mostcommoncond = NULL then
		return n
	end if
	mostcommoncond = mostcommoncond->clone()

	var extracted = astNew(ASTKIND_VEROR)

	'' Extract VERANDs that contain the most common condition
	scope
		var verand = n->head
		while verand
			var verandnext = verand->nxt
			if astIsVERAND(verand) then
				if verand->groupContains(mostcommoncond) then
					extracted->append(verand->clone())
					n->remove(verand)
				end if
			end if
			verand = verandnext
		wend
	end scope

	'' Remove the most common condition from the extracted VERANDs
	scope
		var verand = extracted->head
		while verand
			var verandnext = verand->nxt

			'' Remove common condition from this VERAND
			var cond = verand->head
			while cond
				var condnext = cond->nxt
				if astIsEqual(cond, mostcommoncond, FALSE) then
					verand->remove(cond)
				end if
				cond = condnext
			wend

			'' If this VERAND now only contains 1 condition,
			'' solve out the VERAND
			if verand->head = verand->tail then
				extracted->replace(verand, verand->head->clone())
			end if

			verand = verandnext
		wend
	end scope

	'' Add the common condition on top of the extracted VERANDs
	extracted = astNewVERAND(mostcommoncond, extracted)

	assert(astIsVEROR(n))
	assert(astIsVERAND(extracted))

	'' And re-add that to the original VEROR
	n->append(extracted)
	changed = TRUE

	function = n
end function

private function hBuildVerBop(byval astkind as integer, byval vernum as integer) as AstNode ptr
	function = astNew(astkind, astNewTEXT(frog.versiondefine), astNewTEXT(frog.vernums(vernum)))
end function

private sub hMaybeEmitRangeCheck(byval veror as AstNode ptr, byref l as integer, byref r as integer)
	'' No covered range found (yet)?
	if l < 0 then exit sub

	var l_in_mid = (l > 0)
	var r_in_mid = (r < ubound(frog.vernums))

	'' Covering only 1 or 2 versions, and it isn't at beginning or end (where one of the checks
	'' would become a no-op)?
	var coveredcount = r - l + 1
	if (coveredcount <= 2) and l_in_mid and r_in_mid then
		'' A range check isn't worth it, re-add the normal individual checks
		for i as integer = l to r
			veror->append(astNewVERNUMCHECK(i))
		next
		exit sub
	end if

	'' Build range check, but omit lbound/ubound checks if possible
	'' It's possible that this solves out the entire check (if all versions are covered).
	dim as AstNode ptr check, lcheck, rcheck

	if l_in_mid then lcheck = hBuildVerBop(ASTKIND_GE, l) '' V >= 1
	if r_in_mid then rcheck = hBuildVerBop(ASTKIND_LE, r) '' V <= 1

	if (lcheck <> NULL) and (rcheck <> NULL) then
		check = astNew(ASTKIND_AND, lcheck, rcheck)
	elseif lcheck then
		check = lcheck
	elseif rcheck then
		check = rcheck
	else
		'' All solved out
		exit sub
	end if

	veror->append(check)
end sub

''
'' Visit VERORs and fold things like
''     V=1 or V=2 or V=3
'' into
''     V<=3
''
'' Here no VERORs/VERANDs are solved out (we leave that to hSimplify()), only
'' the atomic conditions are touched.
''
private sub hFoldNumberChecks(byval n as AstNode ptr)
	scope
		var i = n->head
		while i
			hFoldNumberChecks(i)
			i = i->nxt
		wend
	end scope

	if astIsVEROR(n) = FALSE then exit sub

	'' 1. Find & remove all VERNUMCHECKs, set a flag for each covered version
	'' 2. Determine the lbound/ubound of covered versions.
	''    Insert lbound/ubound checks into the VEROR, or re-add VERNUMCHECKs where
	''    optimizations weren't possible.
	''
	'' No-op checks at the absolute lbound/ubound can be omitted though. No need to check V>=1,
	'' if 1 is the minimum supported value anyways.
	''   -declareversions V 1 2 3 4
	''   V=1 or V=2   =   V>=1 and V<=2   =   V<=2
	'' This will also solve out the checks completely if all versions are covered.
	''
	'' Checking lbound/ubound means two checks (V >= lbound and V <= ubound).
	'' Thus it's only really worth using if it covers at least 3 versions:
	''   V=1                 =   V>=1 and V<=1    <- not worth it
	''   V=1 or V=2          =   V>=1 and V<=2    <- not worth it
	''   V=1 or V=2 or V=3   =   V>=1 and V<=3    <- but this is useful
	''
	'' In practice there can be "holes" in the covered versions, for example versions 1, 2, 3
	'' and 5 are covered, but not version 4. In cases like that, there can be multiple "chunks"
	'' with lbound/ubound.
	''   V=1 or V=2 or V=3 or V=5   =   (V>=1 and V<=3) or V=5
	''   V=1 or V=2 or V=3 or V=5 or V=6 or V=7   =   (V>=1 and V<=3) or (V>=5 and V<=7)

	dim covered(any) as byte
	redim covered(0 to ubound(frog.vernums))

	scope
		var i = n->head
		while i
			var nxt = i->nxt

			if i->kind = ASTKIND_VERNUMCHECK then
				covered(i->vernum) = TRUE
				n->remove(i)
			end if

			i = nxt
		wend
	end scope

	#if 0
		print "covered:"
		for i as integer = 0 to ubound(covered)
			print "    " & i & " " + frog.vernums(i) + "  " + iif(covered(i), "yes", "no")
		next
	#endif

	var l = -1, r = -1
	for i as integer = 0 to ubound(covered)
		assert(l <= r)
		if covered(i) then
			if l = -1 then
				l = i
				r = i
			else
				r += 1
			end if
		else
			'' Handle previous chunk, if any
			hMaybeEmitRangeCheck(n, l, r)
			l = -1
			r = -1
		end if
	next

	'' Handle last chunk, if any
	hMaybeEmitRangeCheck(n, l, r)
end sub

private function foldUnix(byval n as AstNode ptr) as AstNode ptr
	if hIsCondition(n) then return n

	scope
		var i = n->head
		while i
			i = n->replace(i, foldUnix(i->clone()))
		wend
	end scope

	if astIsVEROR(n) then
		'' Simplify OS define checks to __FB_UNIX__
		if coversAllUnix(n) then
			replaceUnixChecks(n)
		end if
	end if

	'' Single child, or none at all? Solve out the VEROR/VERAND.
	if n->head = n->tail then
		var child = n->head
		n->unlink(child)
		n = child
	end if

	function = n
end function

private function hSimplifyVersionExpr(byval n as AstNode ptr) as AstNode ptr
	dim as integer changed

	do
		changed = FALSE
		n = hSimplify(n, changed)
	loop while changed

	if ubound(frog.vernums) >= 0 then
		hFoldNumberChecks(n)
	end if

	do
		changed = FALSE
		n = hSimplify(n, changed)
	loop while changed

	n = foldUnix(n)

	function = n
end function

private function hBuildIfConditionFor(byval bits as ApiBits) as AstNode ptr
	dim expr as AstNode ptr

	'' Build VEROR from the VERBLOCK's apis bitmask
	for i as integer = 0 to frog.apicount - 1
		if bits.isSet(i) then
			var api = @frog.apis[i]
			var verand = api->verand->clone()

			'' Conditions for API's target
			if osinfo(api->target.os).has_arm then
				verand->prepend(astNewDEFINEDfbarm(not archinfo(api->target.arch).is_arm))
			end if
			if osinfo(api->target.os).has_64bit then
				verand->prepend(astNewDEFINEDfb64(not archinfo(api->target.arch).is_64bit))
			end if
			if frog.enabledoscount > 1 then
				verand->prepend(astNewDEFINEDfbos(api->target.os))
			end if

			var veror = astNewVEROR(verand)
			if expr then
				expr = astNewVEROR(expr, veror)
			else
				expr = veror
			end if
		end if
	next

	assert(expr)

	'' Beautification: Apply some trivial refactoring to the version
	'' conditions, to eliminate duplicate checks where possible.
	expr = hSimplifyVersionExpr(expr)

	function = expr
end function

private sub hGenVerExprs(byval code as AstNode ptr)
	var i = code->head
	while i
		var inext = i->nxt

		'' Handle #if checks nested inside structs
		hGenVerExprs(i)

		if astIsVERBLOCK(i) then
			i->expr = hBuildIfConditionFor(i->apis)

			'' If we were able to solve it out completely that means
			'' the check was always true -- thus, we can remove this
			'' #if check and insert the body in its place.
			if i->expr = NULL then
				code->replace(i, i->cloneChildren())
			end if
		end if

		i = inext
	wend
end sub

#define astIsPPIFWithVERAND(n) (astIsPPIF(n) andalso astIsVERAND(n->expr))
#define astIsPPELSEIFWithVERAND(n) (astIsPPELSEIF(n) andalso astIsVERAND(n->expr))
#define astIsPPELSEWithVERAND(n) (astIsPPELSE(n) andalso astIsVERAND(n->expr))
#define hasVerandPrefix(n, prefix) astIsEqual(n->expr->head, prefix, FALSE)

private function findEndifOfVerandIfBlockWithCommonPrefix(byval n as AstNode ptr) as AstNode ptr
	if astIsPPIFWithVERAND(n) = FALSE then exit function
	var prefix = n->expr->head
	n = n->nxt

	'' Skip #elseifs if they're ok
	while astIsPPELSEIFWithVERAND(n) andalso hasVerandPrefix(n, prefix)
		n = n->nxt
	wend

	'' Skip #else if it's ok
	if astIsPPELSEWithVERAND(n) andalso hasVerandPrefix(n, prefix) then
		n = n->nxt
	end if

	'' Must have reached #endif. If not, there was an #elseif/#else without
	'' VERAND or with different prefix...
	if astIsPPENDIF(n) = FALSE then exit function
	function = n
end function

''     #if VERAND(a, b)
''     #elseif VERAND(a, c)
''     #else VERAND(a, d)
''     #endif
'' =>
''     #if a
''         #if b
''         #elseif c
''         #else d
''         #endif
''     #endif
private sub splitVerandIfsIntoNestedIfs(byval ast as AstNode ptr)
	var i = ast->head
	while i
		var inext = i->nxt

		var iendif = findEndifOfVerandIfBlockWithCommonPrefix(i)
		if iendif then
			'' New outer #if with just the prefix condition
			var newif = astNew(ASTKIND_PPIF)
			newif->expr = i->expr->head->clone()

			'' Move all the old #if/#elseifs/#else/#endif into the new #if
			inext = iendif->nxt
			newif->takeAndAppendChildSequence(ast, i, iendif)

			'' Insert the new #if in the old one's place
			ast->insert(newif, inext)
			ast->insert(astNew(ASTKIND_PPENDIF), inext)

			'' Cut prefix from the VERANDs of the nested #if/#elseifs/#else
			scope
				var nested = newif->head
				do
					var verand = nested->expr
					verand->remove(verand->head)

					'' Solve out the VERAND if only 1 expression left
					assert(verand->head)
					if verand->head = verand->tail then
						nested->expr = verand->head
						verand->head = NULL
						verand->tail = NULL
						delete verand
					end if

					nested = nested->nxt
				loop until astIsPPENDIF(nested)
			end scope

			'' Visit the new #if, to process its nested #ifs recursively
			inext = newif
		else
			'' Visit children recursively once no more splitting can be done here
			splitVerandIfsIntoNestedIfs(i)
		end if

		i = inext
	wend
end sub

private sub mergeSiblingIfs(byval ast as AstNode ptr)
	var i = ast->head
	while i
		var nxt = i->nxt

		'' Handle #ifs in structs, and nested #ifs (after 1st got merged
		'' into 2nd below, we'll visit the 2nd here, while the 1st was
		'' removed)
		mergeSiblingIfs(i)

		'' #if?
		if astIsPPIF(i) then
			'' #endif?
			var iendif = i->nxt
			if iendif andalso astIsPPENDIF(iendif) then
				'' #if?
				var secondif = iendif->nxt
				if secondif andalso astIsPPIF(secondif) then
					'' Same condition?
					if astIsEqual(i->expr, secondif->expr, FALSE) then
						'' Merge 1st into 2nd
						'' (we know the 1st doesn't have any #elseifs/#else, but the 2nd might)
						secondif->takeAndPrependChildren(i)
						ast->remove(i)
						ast->remove(iendif)
						nxt = secondif
					end if
				end if
			end if
		end if

		i = nxt
	wend
end sub

private sub removeUnnecessaryIfNesting(byval ast as AstNode ptr)
	var i = ast->head
	while i

		'' Handle nested #ifs and #ifs in structs
		removeUnnecessaryIfNesting(i)

		if astIsPPIF(i) then
			'' If this #if contains only another #if/#endif as children...
			var nested = i->head
			if nested andalso astIsPPIF(nested) then
				var nestedendif = nested->nxt
				if nestedendif andalso astIsPPENDIF(nestedendif) then
					if nestedendif = i->tail then
						'' then merge it with its parent:
						''
						''    #if a
						''        #if b
						'' =>
						''    #if VERAND(a, b)
						''
						'' or:
						''
						''    #if a
						''        #if VERAND(b, c)
						'' =>
						''    #if VERAND(a, b, c)
						i->expr = astNewVERAND(i->expr, nested->expr)
						i->head = nested->head
						i->tail = nested->tail
						nested->expr = NULL
						nested->head = NULL
						nested->tail = NULL
						delete nested
						delete nestedendif
					end if
				end if
			end if
		end if

		i = i->nxt
	wend
end sub

''
'' Merge consecutive VERBLOCKs (#if blocks) with VERANDs with common prefix.
''
''    #if defined(__FB_WIN32__) and defined(STATIC)
''    #endif
''    #if defined(__FB_WIN32__) and (not defined(STATIC))
''    #endif
''  =>
''    #ifdef __FB_WIN32__
''        #ifdef STATIC
''        #else
''        #endif
''    #endif
''
'' But verblocks with common prefix mustn't be reordered, i.e. the shortest
'' common prefix must be merged first, and the longest must be last.
''
'' 1. look through consecutive VERBLOCKs with VERANDs
'' 2. collect all those with a common prefix, merge them
'' 3. process recursively
'' 4. repeat until no more common prefix in that bulk of verblocks
''
'' Alternative idea:
''        #if defined(__FB_WIN32__) and defined(STATIC)
''        #endif
''        #if defined(__FB_WIN32__) and (not defined(STATIC))
''        #endif
'' 1. expand all VERANDs as nested #ifs
''        #ifdef __FB_WIN32__
''            #ifdef STATIC
''            #endif
''        #endif
''        #ifdef __FB_WIN32__
''            #ifndef STATIC
''            #endif
''        #endif
'' 2. merge sibling #if blocks with equal condition
''        #ifdef __FB_WIN32__
''            #ifdef STATIC
''            #endif
''            #ifndef STATIC
''            #endif
''        #endif
'' 3. re-run hTurnVerblocksIntoPpIfs to merge #ifdef FOO + #ifndef FOO to #if/#else
''
'' This should be done with VERBLOCKs, not #ifs, to make it simpler. But even
'' with #if expressions generated on VERBLOCKs already, it can't be done,
'' because hTurnVerblocksIntoPpIfs requires the AstNode.apis fields to be set
'' properly and they can't be split up like the #if expressions...
''
private sub combineIfsWithCommonPrefix(byval ast as AstNode ptr)
	splitVerandIfsIntoNestedIfs(ast)
	mergeSiblingIfs(ast)
	removeUnnecessaryIfNesting(ast)
end sub

'' Determine whether two #if conditions are logically opposites
''    A    and    B         maybe, but not necessarily.
''    A    and    not A     are definitely opposite.
private function conditionsAreOpposite(byval a as AstNode ptr, byval b as AstNode ptr) as integer
	if astIsNOT(a) and (not astIsNOT(b)) then return astIsEqual(a->head, b, FALSE)
	if astIsNOT(b) and (not astIsNOT(a)) then return astIsEqual(b->head, a, FALSE)
	function = FALSE
end function

'' Fold #if/elseif to #if/else if possible. Normally this is done by
'' hTurnVerblocksIntoPpIfs() already, but combineIfsWithCommonPrefix() may have
'' produced such cases again, and we can't re-run hTurnVerblocksIntoPpIfs()
'' because that works with VERBLOCKs and AstNode.apis, not with the generated
'' condition expressions.
private sub foldIfElseIf(byval ast as AstNode ptr)
	var i = ast->head
	while i

		foldIfElseIf(i)

		if astIsPPIF(i) andalso astIsPPELSEIF(i->nxt) andalso astIsPPENDIF(i->nxt->nxt) then
			if conditionsAreOpposite(i->expr, i->nxt->expr) then
				i->nxt->kind = ASTKIND_PPELSE
				maybeSwapIfElseWithoutRelinking(i)
			end if
		end if

		i = i->nxt
	wend
end sub

''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''

sub astProcessVerblocks(byval code as AstNode ptr)
	assert(code->kind = ASTKIND_GROUP)
	hSolveOutRedundantVerblocks(code, frog.fullapis)
	hGenVerExprs(code)
	hTurnVerblocksIntoPpIfs(code)
	combineIfsWithCommonPrefix(code)
	foldIfElseIf(code)
end sub
