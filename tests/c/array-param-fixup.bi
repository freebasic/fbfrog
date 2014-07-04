#pragma once

extern "C"

'' @fbfrog -whitespace -nonamefixup

declare sub f(byval i as long ptr)
extern p as sub(byval i as long ptr)
declare sub f(byval p as sub(byval i as long ptr))

type UDT
	declare sub f(byval i as long ptr)
	p as sub(byval i as long ptr)
end type

const A = cptr(sub(byval i as long ptr), 0)

end extern
