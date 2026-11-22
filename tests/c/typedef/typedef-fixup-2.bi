#pragma once

extern "C"

' B shouldn't be typedeffed to A here; since A is a pointer to the struct it
' shouldn't be seen as alias for it. B should be used as the alias.

type B1
	i as long
end type

type A1 as B1 ptr

type B2
	i as long
end type

type A2 as B2 ptr ptr

type B3
	i as long
end type

type A3 as function() as B3

end extern
