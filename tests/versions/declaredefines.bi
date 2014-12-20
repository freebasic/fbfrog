#pragma once

#ifdef A
	dim shared iA as long
#elseif defined(B)
	dim shared iB as long
#else
	dim shared iC as long
#endif
