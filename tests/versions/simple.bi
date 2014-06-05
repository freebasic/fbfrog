#ifndef __VERSION__
	#define __VERSION__ "1"
#endif

#if (__VERSION__ <> "1") and (__VERSION__ <> "2")
	#error "'__VERSION__' is #defined to an unsupported value; expected one of: ""1"", ""2"""
#endif

#pragma once

extern "C"

extern a1 as long

#if __VERSION__ = "1"
	extern a2 as long
#endif

extern a3 as long

end extern
