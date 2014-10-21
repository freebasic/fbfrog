#pragma once

#include once "partial-field.h"
#include once "partial-param.h"

extern "C"

type UDT
	myfield as long
end type

declare sub f(byval myparam as long)

end extern
