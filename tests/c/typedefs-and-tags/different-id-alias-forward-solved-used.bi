#pragma once

extern "C"

type A as B

type B
	a as A ptr
end type

declare sub f(byval as A ptr)
/'

type A as B

type B
	a as A ptr
end type

declare sub f(byval as A ptr)

'/

end extern
