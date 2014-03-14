#ifndef __VERSION__
	#define __VERSION__() "1"
#endif

#if (__VERSION__ <> "1") and (__VERSION__ <> "2")
	#error "'__VERSION__' is #defined to an unsupported value; expected one of: "1", "2""
#endif

#if __VERSION__ = "1"
	extern "Windows"
#else
	extern "C"
#endif

declare sub f1()
declare sub f2()

end extern
