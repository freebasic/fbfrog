#pragma once

extern "C"

extern i as byte
extern p as zstring ptr
extern p as zstring ptr
extern array as zstring * 10
extern array(0 to 9) as zstring * 20

declare sub f(byval i as byte)
declare sub f(byval p as zstring ptr)
declare sub f(byval array as zstring ptr)

type UDT
	i as byte
	p as zstring ptr
	array as zstring * 10
	array(0 to 9) as zstring * 20
end type

#define A1 cbyte(1)
#define A2 cptr(function cdecl() as byte, 1)
#define A3 cptr(sub cdecl(byval as byte), 1)

extern i as const byte
extern p as const zstring ptr
extern p as const zstring ptr
extern array as const zstring * 10
extern array(0 to 9) as const zstring * 20

declare sub f(byval i as const byte)
declare sub f(byval p as const zstring ptr)
declare sub f(byval array as const zstring ptr)

type UDT
	i as const byte
	p as const zstring ptr
	array as const zstring * 10
	array(0 to 9) as const zstring * 20
end type

#define A4 cast(const byte, 1)
#define A5 cptr(function cdecl() as const byte, 1)
#define A6 cptr(sub cdecl(byval as const byte), 1)

extern i as byte
extern p as byte ptr
extern array(0 to 9) as byte
extern array(0 to 9, 0 to 19) as byte

declare sub f(byval i as byte)
declare sub f(byval p as byte ptr)
declare sub f(byval array as byte ptr)

type UDT
	i as byte
	p as byte ptr
	array(0 to 9) as byte
	array(0 to 9, 0 to 19) as byte
end type

#define A7 cbyte(1)
#define A8 cptr(function cdecl() as byte, 1)
#define A9 cptr(sub cdecl(byval as byte), 1)

extern i as const byte
extern p as const byte ptr
extern array(0 to 9) as const byte
extern array(0 to 9, 0 to 19) as const byte

declare sub f(byval i as const byte)
declare sub f(byval p as const byte ptr)
declare sub f(byval array as const byte ptr)

type UDT
	i as const byte
	p as const byte ptr
	array(0 to 9) as const byte
	array(0 to 9, 0 to 19) as const byte
end type

#define A10 cast(const byte, 1)
#define A11 cptr(function cdecl() as const byte, 1)
#define A12 cptr(sub cdecl(byval as const byte), 1)

extern i as ubyte
extern p as ubyte ptr
extern array(0 to 9) as ubyte
extern array(0 to 9, 0 to 19) as ubyte

declare sub f(byval i as ubyte)
declare sub f(byval p as ubyte ptr)
declare sub f(byval array as ubyte ptr)

type UDT
	i as ubyte
	p as ubyte ptr
	array(0 to 9) as ubyte
	array(0 to 9, 0 to 19) as ubyte
end type

#define A13 cubyte(1)
#define A14 cptr(function cdecl() as ubyte, 1)
#define A15 cptr(sub cdecl(byval as ubyte), 1)

extern i as const ubyte
extern p as const ubyte ptr
extern array(0 to 9) as const ubyte
extern array(0 to 9, 0 to 19) as const ubyte

declare sub f(byval i as const ubyte)
declare sub f(byval p as const ubyte ptr)
declare sub f(byval array as const ubyte ptr)

type UDT
	i as const ubyte
	p as const ubyte ptr
	array(0 to 9) as const ubyte
	array(0 to 9, 0 to 19) as const ubyte
end type

#define A16 cast(const ubyte, 1)
#define A17 cptr(function cdecl() as const ubyte, 1)
#define A18 cptr(sub cdecl(byval as const ubyte), 1)

end extern
