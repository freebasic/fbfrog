#pragma once

extern "C"

sub f1()
end sub
sub f2()
	extern     i alias "i" as long
	dim shared i as long = 1
	printf(!"%i\n", 1)
end sub

end extern
