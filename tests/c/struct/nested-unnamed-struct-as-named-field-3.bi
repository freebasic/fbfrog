type __dummyid0
end type

#ifdef __FB_WIN32__
	type __dummyid4 as __dummyid0
#elseif defined( __FB_LINUX__ )
	type __dummyid2 as __dummyid0
#endif

type __dummyid1
	union
		#ifdef __FB_DOS__
			a as __dummyid0
		#elseif defined( __FB_LINUX__ )
			a as __dummyid2
		#else
			a as __dummyid4
		#endif
	end union
end type

#ifdef __FB_WIN32__
	type __dummyid5 as __dummyid1
#elseif defined( __FB_LINUX__ )
	type __dummyid3 as __dummyid1
#endif

type UDT
	union
		#ifdef __FB_DOS__
			b as __dummyid1
		#elseif defined( __FB_LINUX__ )
			b as __dummyid3
		#else
			b as __dummyid5
		#endif
	end union
end type
