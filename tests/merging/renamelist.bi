#pragma once

#ifndef VER
	#define VER 2
#endif

#if VER = 1
#elseif VER = 2
#else
	#error "'VER' is #defined to an unsupported value; expected one of: 1, 2"
#endif

'' The following symbols have been renamed:
''     procedure string_ alias "string"()
''     #if VER = 1
''         procedure single_ alias "single"()
''     #else
''         procedure integer_ alias "integer"()
''     #endif

extern "C"

declare sub string_ alias "string"()

#if VER = 1
	declare sub single_ alias "single"()
#else
	declare sub integer_ alias "integer"()
#endif

end extern
