#pragma once

extern "C"

'' @fbfrog -whitespace -nonamefixup

extern i as byte
extern p as zstring ptr
extern p as zstring ptr
extern array(0 to 10 - 1) as zstring
declare sub f(byval i as byte)
declare sub f(byval p as zstring ptr)
declare sub f(byval array as zstring ptr)
type UDT
	i as byte
	p as zstring ptr
	array(0 to 10 - 1) as zstring
end type

extern i as const byte
extern p as const zstring ptr
extern p as const zstring ptr
extern array(0 to 10 - 1) as const zstring
declare sub f(byval i as const byte)
declare sub f(byval p as const zstring ptr)
declare sub f(byval array as const zstring ptr)
type UDT
	i as const byte
	p as const zstring ptr
	array(0 to 10 - 1) as const zstring
end type

extern i as byte
extern p as byte ptr
extern array(0 to 10 - 1) as byte
declare sub f(byval i as byte)
declare sub f(byval p as byte ptr)
declare sub f(byval array as byte ptr)
type UDT
	i as byte
	p as byte ptr
	array(0 to 10 - 1) as byte
end type

extern i as const byte
extern p as const byte ptr
extern array(0 to 10 - 1) as const byte
declare sub f(byval i as const byte)
declare sub f(byval p as const byte ptr)
declare sub f(byval array as const byte ptr)
type UDT
	i as const byte
	p as const byte ptr
	array(0 to 10 - 1) as const byte
end type

extern i as ubyte
extern p as ubyte ptr
extern array(0 to 10 - 1) as ubyte
declare sub f(byval i as ubyte)
declare sub f(byval p as ubyte ptr)
declare sub f(byval array as ubyte ptr)
type UDT
	i as ubyte
	p as ubyte ptr
	array(0 to 10 - 1) as ubyte
end type

extern i as const ubyte
extern p as const ubyte ptr
extern array(0 to 10 - 1) as const ubyte
declare sub f(byval i as const ubyte)
declare sub f(byval p as const ubyte ptr)
declare sub f(byval array as const ubyte ptr)
type UDT
	i as const ubyte
	p as const ubyte ptr
	array(0 to 10 - 1) as const ubyte
end type
const A = cptr(sub(byval as const ubyte), 1)

end extern
