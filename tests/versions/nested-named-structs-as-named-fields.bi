#ifndef __VERSION__
	#define __VERSION__ "1"
#endif

#if (__VERSION__ <> "1") and (__VERSION__ <> "2")
	#error "'__VERSION__' is #defined to an unsupported value; expected one of: "1", "2""
#endif

type __dummyid0
	#if __VERSION__ = "1"
		v1_a as long
	#else
		v2_a as long
	#endif
end type

#if __VERSION__ = "2"
	#ifdef __FB_WIN32__
		type __dummyid8 as __dummyid0
	#elseif defined( __FB_LINUX__ )
		type __dummyid7 as __dummyid0
	#else
		type __dummyid6 as __dummyid0
	#endif
#else
	#ifdef __FB_WIN32__
		type __dummyid4 as __dummyid0
	#elseif defined( __FB_LINUX__ )
		type __dummyid2 as __dummyid0
	#endif

	type __dummyid1
		v1_b as long
	end type

	#ifdef __FB_WIN32__
		type __dummyid5 as __dummyid1
	#elseif defined( __FB_LINUX__ )
		type __dummyid3 as __dummyid1
	#endif
#endif

type UDT
	#if __VERSION__ = "1"
		#ifdef __FB_DOS__
			a as __dummyid0
			b as __dummyid1
		#elseif defined( __FB_LINUX__ )
			a as __dummyid2
			b as __dummyid3
		#else
			a as __dummyid4
			b as __dummyid5
		#endif
	#else
		#ifdef __FB_DOS__
			a as __dummyid6
		#elseif defined( __FB_LINUX__ )
			a as __dummyid7
		#else
			a as __dummyid8
		#endif
	#endif
end type
