#pragma once

extern "C"

type B as B_
type A as B
declare sub f1(byval as A ptr)
declare sub f2(byval as B ptr)

type B_
	a as A ptr
end type

declare sub f3(byval as A ptr)
declare sub f4(byval as B ptr)

end extern
