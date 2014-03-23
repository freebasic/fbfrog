extern "C"

type __dummyid0
	a as long
end type

type __dummyid1
	a as long
end type

type __dummyid2
	a as long
end type

type __dummyid3
	a as long
end type

type UDT
	a as __dummyid0
	b as __dummyid1
	c as __dummyid1
	d as __dummyid1
	e as __dummyid2 ptr
	f as function() as __dummyid3
end type

end extern
