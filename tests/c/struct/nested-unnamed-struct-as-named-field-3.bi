#ifdef __FB_DOS__
	type __fbfrog_anon0
	end type

	type __fbfrog_anon1
		union
			a as __fbfrog_anon0
		end union
	end type
#elseif defined( __FB_LINUX__ )
	type __fbfrog_anon2
	end type

	type __fbfrog_anon3
		union
			a as __fbfrog_anon2
		end union
	end type
#else
	type __fbfrog_anon4
	end type

	type __fbfrog_anon5
		union
			a as __fbfrog_anon4
		end union
	end type
#endif

type UDT
	union
		#ifdef __FB_DOS__
			b as __fbfrog_anon1
		#elseif defined( __FB_LINUX__ )
			b as __fbfrog_anon3
		#else
			b as __fbfrog_anon5
		#endif
	end union
end type
