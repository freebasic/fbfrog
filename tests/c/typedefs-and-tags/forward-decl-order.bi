#pragma once

extern "C"

type A as A_
type B as B_

type UDT
	aa as A ptr
	bb as B ptr
end type

type C as C_
type D as D_
declare sub f(byval cc as C ptr, byval dd as D ptr)

type A_
	i as long
end type

type B_
	i as long
end type

type C_
	i as long
end type

type D_
	i as long
end type

end extern
