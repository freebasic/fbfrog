#pragma once

'' The following symbols have been renamed:
''     procedure string => string_
''     #if VER = 1
''         procedure single => single_
''     #else
''         procedure integer => integer_
''     #endif

extern "C"

declare sub string_ alias "string"()

#if VER = 1
	declare sub single_ alias "single"()
#else
	declare sub integer_ alias "integer"()
#endif

end extern
