#pragma once

#inclib "main"

#if VER = 1
	#inclib "cool1"
#else
	#inclib "cool-2.0"
#endif

extern "C"

declare sub f()

end extern
