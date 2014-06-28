#ifdef A
#elseif defined(B)
#elseif defined(C)
#else
	#error "Not one of these symbols is #defined: A, B, C"
#endif

#pragma once

#ifdef A
	dim shared iA as long
#elseif defined(B)
	dim shared iB as long
#else
	dim shared iC as long
#endif
