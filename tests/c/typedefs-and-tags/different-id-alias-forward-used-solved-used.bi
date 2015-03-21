#pragma once

extern "C"

type A as B
declare sub f1(byval as A ptr)

type B
	a as A ptr
end type

declare sub f2(byval as A ptr)

end extern
