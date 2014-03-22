extern "C"

type __dummyid0
	a as long
end type

#ifdef __FB_WIN32__
	type __dummyid8 as __dummyid0
#elseif defined( __FB_LINUX__ )
	type __dummyid4 as __dummyid0
#endif

type __dummyid1
	a as long
end type

#ifdef __FB_WIN32__
	type __dummyid9 as __dummyid1
#elseif defined( __FB_LINUX__ )
	type __dummyid5 as __dummyid1
#endif

type __dummyid2
	a as long
end type

#ifdef __FB_WIN32__
	type __dummyid10 as __dummyid2
#elseif defined( __FB_LINUX__ )
	type __dummyid6 as __dummyid2
#endif

type __dummyid3
	a as long
end type

#ifdef __FB_WIN32__
	type __dummyid11 as __dummyid3
#elseif defined( __FB_LINUX__ )
	type __dummyid7 as __dummyid3
#endif

type UDT
	#ifdef __FB_DOS__
		a as __dummyid0
		b as __dummyid1
		c as __dummyid1
		d as __dummyid1
		e as __dummyid2 ptr
		f as function() as __dummyid3
	#elseif defined( __FB_LINUX__ )
		a as __dummyid4
		b as __dummyid5
		c as __dummyid5
		d as __dummyid5
		e as __dummyid6 ptr
		f as function() as __dummyid7
	#else
		a as __dummyid8
		b as __dummyid9
		c as __dummyid9
		d as __dummyid9
		e as __dummyid10 ptr
		f as function() as __dummyid11
	#endif
end type

end extern
