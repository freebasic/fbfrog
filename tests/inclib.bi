#ifndef __VERSION__
	#define __VERSION__ "1"
#endif

#if (__VERSION__ <> "1") and (__VERSION__ <> "2")
	#error "'__VERSION__' is #defined to an unsupported value; expected one of: ""1"", ""2"""
#endif

#inclib "main"

#if __VERSION__ = "1"
	#inclib "cool1"
#else
	#inclib "cool-2.0"
#endif

extern "C"

declare sub f()

end extern
