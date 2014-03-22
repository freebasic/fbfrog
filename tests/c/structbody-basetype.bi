extern "C"

#ifdef __FB_DOS__
	type __dummyid0
		a as long
	end type

	declare function f1() as __dummyid0
#elseif defined( __FB_LINUX__ )
	type __dummyid7
		a as long
	end type

	declare function f1() as __dummyid7
#else
	type __dummyid14
		a as long
	end type

	declare function f1() as __dummyid14
#endif

type UDT2
	a as long
end type

declare function f2() as UDT2

#ifdef __FB_DOS__
	type __dummyid1
		a as long
	end type

	declare function f3() as __dummyid1
#elseif defined( __FB_LINUX__ )
	type __dummyid8
		a as long
	end type

	declare function f3() as __dummyid8
#else
	type __dummyid15
		a as long
	end type

	declare function f3() as __dummyid15
#endif

type UDT4
	a as long
end type

declare function f4() as UDT4

#ifdef __FB_DOS__
	type __dummyid2
		a as long
	end type

	dim shared a5 as __dummyid2
	dim shared b5 as __dummyid2
#elseif defined( __FB_LINUX__ )
	type __dummyid9
		a as long
	end type

	dim shared a5 as __dummyid9
	dim shared b5 as __dummyid9
#else
	type __dummyid16
		a as long
	end type

	dim shared a5 as __dummyid16
	dim shared b5 as __dummyid16
#endif

type UDT6
	a as long
end type

dim shared a6 as UDT6
dim shared b6 as UDT6

#ifdef __FB_DOS__
	union __dummyid3
		a as long
	end union

	declare function f7() as __dummyid3
#elseif defined( __FB_LINUX__ )
	union __dummyid10
		a as long
	end union

	declare function f7() as __dummyid10
#else
	union __dummyid17
		a as long
	end union

	declare function f7() as __dummyid17
#endif

union UDT8
	a as long
end union

declare function f8() as UDT8

#ifdef __FB_DOS__
	enum __dummyid4
		A = 0
	end enum

	declare function f9() as __dummyid4
#elseif defined( __FB_LINUX__ )
	enum __dummyid11
		A = 0
	end enum

	declare function f9() as __dummyid11
#else
	enum __dummyid18
		A = 0
	end enum

	declare function f9() as __dummyid18
#endif

enum UDT10
	B = 0
end enum

declare function f10() as UDT10

#ifdef __FB_DOS__
	type __dummyid5
		a as long
	end type
#elseif defined( __FB_LINUX__ )
	type __dummyid12
		a as long
	end type
#else
	type __dummyid19
		a as long
	end type
#endif

type UDT12
	a as long
end type

#ifdef __FB_DOS__
	type __dummyid6
		a as long
	end type
#elseif defined( __FB_LINUX__ )
	type __dummyid13
		a as long
	end type
#else
	type __dummyid20
		a as long
	end type
#endif

type UDT11
	#ifdef __FB_DOS__
		field1 as __dummyid5
	#elseif defined( __FB_LINUX__ )
		field1 as __dummyid12
	#else
		field1 as __dummyid19
	#endif

	field2 as UDT12

	#ifdef __FB_DOS__
		a as const __dummyid6
		b as const __dummyid6
	#elseif defined( __FB_LINUX__ )
		a as const __dummyid13
		b as const __dummyid13
	#else
		a as const __dummyid20
		b as const __dummyid20
	#endif
end type

end extern
