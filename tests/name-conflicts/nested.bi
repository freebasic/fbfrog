extern "C"

#define GLOBALDEFINE1_ 1

declare sub globalproc()

type globaltype as long

type UDT1
	globaldefine1 as long
	globaldefine2 as long
	globalproc as long
	globaltype as long
	foo as long
	FOO_ as long

	union
		Foo__ as long
		fOO___ as long
	end union

	as_ as long
	IF_ as long
end type

declare sub f1(byval globaldefine1_ as long, byval globaldefine2 as long, byval globalproc as long, byval globaltype as long, byval foo as long, byval FOO_ as long, byval as_ as long, byval IF_ as long)

#define GLOBALDEFINE2 1

type UDT2
	foo as long
end type

declare sub f2(byval foo as long)

end extern
