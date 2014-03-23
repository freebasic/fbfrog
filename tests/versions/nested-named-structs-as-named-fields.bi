#ifndef __VERSION__
	#define __VERSION__ "1"
#endif

#if (__VERSION__ <> "1") and (__VERSION__ <> "2")
	#error "'__VERSION__' is #defined to an unsupported value; expected one of: "1", "2""
#endif

#if __VERSION__ = "1"
	type __dummyid0
		v1_a as long
	end type
#endif

type __dummyid1
	#if __VERSION__ = "1"
		v1_b as long
	#else
		v2_a as long
	#endif
end type

type UDT
	#if __VERSION__ = "1"
		a as __dummyid0
		b as __dummyid1
	#else
		a as __dummyid1
	#endif
end type
