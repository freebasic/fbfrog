type __dummyid0
end type

type __dummyid1
	union
		a as __dummyid0
	end union
end type

type UDT
	union
		b as __dummyid1
	end union
end type
