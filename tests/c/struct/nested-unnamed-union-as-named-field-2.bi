#ifdef __FB_DOS__
	union __fbfrog_anon0
		a as long
	end union
#elseif defined( __FB_LINUX__ )
	union __fbfrog_anon1
		a as long
	end union
#else
	union __fbfrog_anon2
		a as long
	end union
#endif

type UDT
	union
		type
			#ifdef __FB_DOS__
				a as __fbfrog_anon0
			#elseif defined( __FB_LINUX__ )
				a as __fbfrog_anon1
			#else
				a as __fbfrog_anon2
			#endif
		end type
	end union
end type
