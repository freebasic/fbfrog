#pragma once

'' The following symbols have been renamed:
''     inside struct UDT1:
''         field FOO => FOO_
''         field Foo => Foo__
''         field fOO => fOO___
''     #define GLOBALDEFINE1 => GLOBALDEFINE1_
''     #define GLOBALCONSTANT1 => GLOBALCONSTANT1_

extern "C"

#define GLOBALDEFINE1_() 1
#define GLOBALCONSTANT1_ 1

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
	as function(byval PTR_ as function(byval INT_ as long) as long) as long CONST
end type

declare sub f1(byval globaldefine1_ as long, byval globaldefine2 as long, byval globalconstant1_ as long, byval globalconstant2 as long, byval globalproc as long, byval globaltype as long, byval foo as long, byval FOO_ as long, byval as_ as long, byval IF_ as long, byval PTR_ as function(byval INT_ as long) as long, byval CONST_ as function(byval PTR_ as function(byval INT_ as long) as long) as long)

#define GLOBALDEFINE2() 1
#define GLOBALCONSTANT2 1

type UDT2
	foo as long
end type

declare sub f2(byval foo as long)

end extern
