extern "C"

#ifdef __FB_DOS__
	type __fbfrog_anon0
		a as long
	end type

	type __fbfrog_anon1
		a as long
	end type

	type __fbfrog_anon2
		a as long
	end type

	type __fbfrog_anon3
		a as long
	end type
#elseif defined( __FB_LINUX__ )
	type __fbfrog_anon4
		a as long
	end type

	type __fbfrog_anon5
		a as long
	end type

	type __fbfrog_anon6
		a as long
	end type

	type __fbfrog_anon7
		a as long
	end type
#else
	type __fbfrog_anon8
		a as long
	end type

	type __fbfrog_anon9
		a as long
	end type

	type __fbfrog_anon10
		a as long
	end type

	type __fbfrog_anon11
		a as long
	end type
#endif

type UDT
	#ifdef __FB_DOS__
		a as __fbfrog_anon0
		b as __fbfrog_anon1
		c as __fbfrog_anon1
		d as __fbfrog_anon1
		e as __fbfrog_anon2 ptr
		f as function() as __fbfrog_anon3
	#elseif defined( __FB_LINUX__ )
		a as __fbfrog_anon4
		b as __fbfrog_anon5
		c as __fbfrog_anon5
		d as __fbfrog_anon5
		e as __fbfrog_anon6 ptr
		f as function() as __fbfrog_anon7
	#else
		a as __fbfrog_anon8
		b as __fbfrog_anon9
		c as __fbfrog_anon9
		d as __fbfrog_anon9
		e as __fbfrog_anon10 ptr
		f as function() as __fbfrog_anon11
	#endif
end type

end extern
