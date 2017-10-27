#include once "ast-match.bi"

function ParentChildPattern.matches(byval parent as AstNode ptr, byval child as AstNode ptr) as integer
	function = strMatch(*parent->text, parentpattern) andalso _
	           strMatch(*child->text, childpattern)
end function

function IndexPattern.matches(byval parentid as zstring ptr, byval childindex as integer) as integer
	function = (this.childindex = childindex) andalso _
	           strMatch(*parentid, parentpattern)
end function

''
'' Declaration pattern format:
''    [<parent-id-pattern>.]<child-id-pattern>
''    <parent-id-pattern>.<child-index>
''
'' "child" id patterns can be handles using a StringMatcher.
''
'' "parent.child" patterns are more difficult: We can use two StringMatchers to
'' track parent and child ids to fairly quickly look for possible matches, but
'' then we still have to check a list of the "parent.child" patterns to
'' determine exactly whether the given parent/child combination matches.
''
'' Same for index patterns.
''
'' TODO: match based on astkind to speed things up a bit
''       (if we have a parentpattern, the child can only be a field/param/enumconst)
''
sub DeclPatterns.parseAndAdd(byref s as string)
	dim as string parent, child
	strSplit(s, ".", parent, child)

	'' Index pattern?
	if (len(parent) > 0) and (len(child) > 0) then
		if strIsNumber(child) then
			indexParents.addPattern(parent)
			var i = indexCount
			indexCount += 1
			index = reallocate(index, sizeof(*index) * indexCount)
			index[i].constructor()
			index[i].parentpattern = parent
			index[i].childindex = valuint(child)
		else
			pcParents.addPattern(parent)
			pcChildren.addPattern(child)
			var i = pccount
			pccount += 1
			pcs = reallocate(pcs, sizeof(*pcs) * pccount)
			pcs[i].constructor()
			pcs[i].parentpattern = parent
			pcs[i].childpattern = child
		end if
	else
		ids.addPattern(s)
	end if
end sub

destructor DeclPatterns()
	for i as integer = 0 to pccount - 1
		pcs[i].destructor()
	next
	deallocate(pcs)
	for i as integer = 0 to indexcount - 1
		index[i].destructor()
	next
	deallocate(index)
end destructor

private function determineParentId(byval parentparent as AstNode ptr, byval parent as AstNode ptr) as zstring ptr
	'' If it's an anonymous procptr subtype, check its parent's id instead
	if parentparent andalso _
	   (parent->kind = ASTKIND_PROC) andalso _
	   (parentparent->subtype = parent) then
		function = parentparent->text
	else
		function = parent->text
	end if
end function

function DeclPatterns.matches _
	( _
		byval parentparent as AstNode ptr, _
		byval parent as AstNode ptr, _
		byval child as AstNode ptr, _
		byval childindex as integer _
	) as integer

	if child->text then
		'' Match against "id" patterns
		if ids.matches(child->text) then
			return TRUE
		end if

		if parent andalso parent->text then
			'' Match against "parent.child" patterns
			'' Potential match?
			if pcParents.matches(parent->text) andalso _
			   pcChildren.matches(child->text) then
				'' Confirm match...
				for i as integer = 0 to pccount - 1
					if pcs[i].matches(parent, child) then
						return TRUE
					end if
				next
			end if
		end if
	end if

	if parent then
		'' Match against index patterns
		'' TODO: only if it's a field/param
		'' Potential match?
		var parentid = determineParentId(parentparent, parent)
		if parentid andalso indexParents.matches(parentid) then
			'' Confirm match...
			for i as integer = 0 to indexcount - 1
				if index[i].matches(parentid, childindex) then
					return TRUE
				end if
			next
		end if
	end if

	function = FALSE
end function
