#ifdef __FB_DOS__
	type __dummyid0
		a as long
	end type
#elseif defined( __FB_LINUX__ )
	type __dummyid1
		a as long
	end type
#else
	type __dummyid2
		a as long
	end type
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
