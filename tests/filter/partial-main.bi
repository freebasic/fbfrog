#pragma once

#include once "partial-field.bi"
#include once "partial-param.bi"

extern "C"

type UDT
	myfield as long
end type

declare sub f(byval myparam as long)

end extern
