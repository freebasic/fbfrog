#define A
#pragma push_macro("A")
#pragma pop_macro("B")

#ifndef A
	#error
#endif

#ifdef B
	#error
#endif
