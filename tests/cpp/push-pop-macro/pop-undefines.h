#pragma push_macro("A")

#define A
#ifndef A
	#error
#endif

#pragma pop_macro("A")
#ifdef A
	#error
#endif
