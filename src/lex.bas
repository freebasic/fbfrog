#include once "lex.bi"

sub LexContext.initKeywords(byval first as integer, byval last as integer)
	for i as integer = first to last
		keywords.addOverwrite(tkInfoText(i), cast(any ptr, i))
	next
end sub

function LexContext.lookupKeyword(byval id as zstring ptr, byval defaulttk as integer) as integer
	var item = keywords.lookup(id, hashHash(id))
	if item->s then
		'' Return the corresponding KW_* (C keyword) or OPT_* (command line option)
		function = cint(item->data)
	else
		function = defaulttk
	end if
end function

sub LexContext.showErrorAndAbort(byref message as string)
	oopsLocation(location, message)
end sub

sub LexContext.setLocation(byval flags as integer = 0)
	tk->setLocation(x, sourcectx->encode(location))
	if behindspace then
		flags or= TKFLAG_BEHINDSPACE
	end if
	tk->addFlags(x, x, flags)
	x += 1
end sub

sub LexContext.newLine()
	location.linenum += 1
end sub
