#ifndef __TEST_VERSION__
	#define __TEST_VERSION__ 1
#endif

#if (__TEST_VERSION__ <> 1) and (__TEST_VERSION__ <> 2)
	#error "'__TEST_VERSION__' is #defined to an unsupported value; expected one of: 1, 2"
#endif

#if __TEST_VERSION__ = 1
	extern "Windows"
#else
	extern "C"
#endif

declare sub f1()
declare sub f2()

end extern
