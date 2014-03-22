union __dummyid0
	a as long
end union

#ifdef __FB_WIN32__
	type __dummyid2 as __dummyid0
#elseif defined( __FB_LINUX__ )
	type __dummyid1 as __dummyid0
#endif

type UDT
	#ifdef __FB_DOS__
		a as __dummyid0
	#elseif defined( __FB_LINUX__ )
		a as __dummyid1
	#else
		a as __dummyid2
	#endif
end type
