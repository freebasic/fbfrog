#include once "util.bi"

function min(byval a as integer, byval b as integer) as integer
	if b < a then a = b
	function = a
end function

function max(byval a as integer, byval b as integer) as integer
	if b > a then a = b
	function = a
end function

sub oops(byval message as zstring ptr)
	print "oops, " + *message
	end 1
end sub
