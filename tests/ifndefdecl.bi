#pragma once

' @fbfrog -ifndefdecl NULL -ifndefdecl TRUE -ifndefdecl FALSE -ifndefdecl M1 -ifndefdecl M2 -ifndefdecl M3 -undefbeforedecl M3
#ifndef NULL
	const NULL = 0
#endif
#ifndef TRUE
	const TRUE = 1
#endif
#ifndef FALSE
	const FALSE = 0
#endif
#ifndef M1
	const M1 = 123
#endif
#undef M2
#ifndef M2
	const M2 = 123
#endif
#undef M3
#ifndef M3
	const M3 = 123
#endif
