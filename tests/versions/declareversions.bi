#pragma once

#if VER = 1
	dim shared i1 as long
#elseif VER = 2
	dim shared i2 as long
#else
	dim shared i3 as long
#endif
