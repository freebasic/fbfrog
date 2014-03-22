#ifndef __VERSION__
	#define __VERSION__ "1"
#endif

#if (__VERSION__ <> "1") and (__VERSION__ <> "2")
	#error "'__VERSION__' is #defined to an unsupported value; expected one of: "1", "2""
#endif

#if __VERSION__ = "1"
	#ifdef __FB_DOS__
		type __dummyid0
			v1_a as long
		end type

		type __dummyid1
			v1_b as long
		end type
	#elseif defined( __FB_LINUX__ )
		type __dummyid2
			v1_a as long
		end type

		type __dummyid3
			v1_b as long
		end type
	#else
		type __dummyid4
			v1_a as long
		end type

		type __dummyid5
			v1_b as long
		end type
	#endif
#else
	#ifdef __FB_DOS__
		type __dummyid6
			v2_a as long
		end type
	#elseif defined( __FB_LINUX__ )
		type __dummyid7
			v2_a as long
		end type
	#else
		type __dummyid8
			v2_a as long
		end type
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
