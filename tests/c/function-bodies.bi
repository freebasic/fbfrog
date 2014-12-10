#pragma once

extern "C"

sub f1()
end sub

sub f2()
	dim i as long = 1
	printf(!"%i\n", 1)
end sub

function f3(byval i as long) as long
	return (i shl 3) and &hFF
	return 0
end function

function f(byval as long) as long
	return 0
end function

end extern
