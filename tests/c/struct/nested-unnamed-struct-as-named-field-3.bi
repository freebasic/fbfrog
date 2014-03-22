#ifdef __FB_DOS__
	type __dummyid0
	end type

	type __dummyid1
		union
			a as __dummyid0
		end union
	end type
#elseif defined( __FB_LINUX__ )
	type __dummyid2
	end type

	type __dummyid3
		union
			a as __dummyid2
		end union
	end type
#else
	type __dummyid4
	end type

	type __dummyid5
		union
			a as __dummyid4
		end union
	end type
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
