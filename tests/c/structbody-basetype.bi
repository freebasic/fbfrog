extern "C"

#ifdef __FB_DOS__
	type __fbfrog_anon0
		a as long
	end type

	declare function f1() as __fbfrog_anon0
#elseif defined( __FB_LINUX__ )
	type __fbfrog_anon7
		a as long
	end type

	declare function f1() as __fbfrog_anon7
#else
	type __fbfrog_anon14
		a as long
	end type

	declare function f1() as __fbfrog_anon14
#endif

type UDT2
	a as long
end type

declare function f2() as UDT2

#ifdef __FB_DOS__
	type __fbfrog_anon1
		a as long
	end type

	declare function f3() as __fbfrog_anon1
#elseif defined( __FB_LINUX__ )
	type __fbfrog_anon8
		a as long
	end type

	declare function f3() as __fbfrog_anon8
#else
	type __fbfrog_anon15
		a as long
	end type

	declare function f3() as __fbfrog_anon15
#endif

type UDT4
	a as long
end type

declare function f4() as UDT4

#ifdef __FB_DOS__
	type __fbfrog_anon2
		a as long
	end type

	dim shared a5 as __fbfrog_anon2
	dim shared b5 as __fbfrog_anon2
#elseif defined( __FB_LINUX__ )
	type __fbfrog_anon9
		a as long
	end type

	dim shared a5 as __fbfrog_anon9
	dim shared b5 as __fbfrog_anon9
#else
	type __fbfrog_anon16
		a as long
	end type

	dim shared a5 as __fbfrog_anon16
	dim shared b5 as __fbfrog_anon16
#endif

type UDT6
	a as long
end type

dim shared a6 as UDT6
dim shared b6 as UDT6

#ifdef __FB_DOS__
	union __fbfrog_anon3
		a as long
	end union

	declare function f7() as __fbfrog_anon3
#elseif defined( __FB_LINUX__ )
	union __fbfrog_anon10
		a as long
	end union

	declare function f7() as __fbfrog_anon10
#else
	union __fbfrog_anon17
		a as long
	end union

	declare function f7() as __fbfrog_anon17
#endif

union UDT8
	a as long
end union

declare function f8() as UDT8

#ifdef __FB_DOS__
	enum __fbfrog_anon4
		A = 0
	end enum

	declare function f9() as __fbfrog_anon4
#elseif defined( __FB_LINUX__ )
	enum __fbfrog_anon11
		A = 0
	end enum

	declare function f9() as __fbfrog_anon11
#else
	enum __fbfrog_anon18
		A = 0
	end enum

	declare function f9() as __fbfrog_anon18
#endif

enum UDT10
	B = 0
end enum

declare function f10() as UDT10

#ifdef __FB_DOS__
	type __fbfrog_anon5
		a as long
	end type
#elseif defined( __FB_LINUX__ )
	type __fbfrog_anon12
		a as long
	end type
#else
	type __fbfrog_anon19
		a as long
	end type
#endif

type UDT12
	a as long
end type

#ifdef __FB_DOS__
	type __fbfrog_anon6
		a as long
	end type
#elseif defined( __FB_LINUX__ )
	type __fbfrog_anon13
		a as long
	end type
#else
	type __fbfrog_anon20
		a as long
	end type
#endif

type UDT11
	#ifdef __FB_DOS__
		field1 as __fbfrog_anon5
	#elseif defined( __FB_LINUX__ )
		field1 as __fbfrog_anon12
	#else
		field1 as __fbfrog_anon19
	#endif

	field2 as UDT12

	#ifdef __FB_DOS__
		a as const __fbfrog_anon6
		b as const __fbfrog_anon6
	#elseif defined( __FB_LINUX__ )
		a as const __fbfrog_anon13
		b as const __fbfrog_anon13
	#else
		a as const __fbfrog_anon20
		b as const __fbfrog_anon20
	#endif
end type

end extern
