#pragma once

#include once "field.bi"
#include once "param.bi"

extern "C"

type UDT
	myfield as long
end type

declare sub f(byval myparam as long)

end extern
