#pragma once

extern "C"

type A
	i as long
end type

declare sub A()
#define A_ A

type B
	i as long
end type

#define B_ B
declare sub B()
#define C_ C

type C
	i as long
end type

declare sub C()

end extern
