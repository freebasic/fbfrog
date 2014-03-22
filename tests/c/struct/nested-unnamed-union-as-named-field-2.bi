#ifdef __FB_DOS__
	union __dummyid0
		a as long
	end union
#elseif defined( __FB_LINUX__ )
	union __dummyid1
		a as long
	end union
#else
	union __dummyid2
		a as long
	end union
#endif

type UDT
	union
		type
			#ifdef __FB_DOS__
				a as __dummyid0
			#elseif defined( __FB_LINUX__ )
				a as __dummyid1
			#else
				a as __dummyid2
			#endif
		end type
	end union
end type
