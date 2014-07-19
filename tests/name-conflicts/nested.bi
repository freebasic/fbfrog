#pragma once

'' The following symbols have been renamed:
''     inside struct UDT1:
''         field FOO_ alias "FOO"
''         field Foo__ alias "Foo"
''         field fOO___ alias "fOO"
''         field PTR_ alias "PTR"
''     #define GLOBALDEFINE1_ alias "GLOBALDEFINE1"

extern "C"

#define GLOBALDEFINE1_() 1

const GLOBALCONSTANT1 = 1

declare sub globalproc()

type globaltype as long
type p as function(byval as_ as long) as long

type UDT1
	globaldefine1 as long
	globaldefine2 as long
	globalconstant1 as long
	globalconstant2 as long
	globalproc as long
	globaltype as long
	foo as long
	FOO_ as long

	union
		Foo__ as long
		fOO___ as long
	end union

	as long as
	IF as long
	PTR as function(byval INT_ as long) as long
	PTR_ as function(byval PTR_ as function(byval INT_ as long) as long) as long
end type

declare sub f1(byval globaldefine1_ as long, byval globaldefine2 as long, byval globalconstant1 as long, byval globalconstant2 as long, byval globalproc as long, byval globaltype as long, byval foo as long, byval FOO_ as long, byval as_ as long, byval IF_ as long, byval PTR_ as function(byval INT_ as long) as long, byval PTR__ as function(byval PTR_ as function(byval INT_ as long) as long) as long)

#define GLOBALDEFINE2() 1

const GLOBALCONSTANT2 = 1

type UDT2
	foo as long
end type

declare sub f2(byval foo as long)

end extern
