#ifdef __FB_DOS__
	type __fbfrog_anon0
		a as long
	end type
#elseif defined( __FB_LINUX__ )
	type __fbfrog_anon1
		a as long
	end type
#else
	type __fbfrog_anon2
		a as long
	end type
#endif

type UDT
	#ifdef __FB_DOS__
		a as __fbfrog_anon0
	#elseif defined( __FB_LINUX__ )
		a as __fbfrog_anon1
	#else
		a as __fbfrog_anon2
	#endif
end type
