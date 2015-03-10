#pragma once

extern "C"

extern i as wchar_t
extern p as wstring ptr
extern p as wstring ptr
extern array as wstring * 10
extern array(0 to 9) as wstring * 20

declare sub f(byval i as wchar_t)
declare sub f(byval p as wstring ptr)
declare sub f(byval array as wstring ptr)

type UDT
	i as wchar_t
	p as wstring ptr
	array as wstring * 10
	array(0 to 9) as wstring * 20
end type

const A1 = cast(wchar_t, 1)
const A2 = cptr(function() as wchar_t, 1)
const A3 = cptr(sub(byval as wchar_t), 1)

extern i as const wchar_t
extern p as const wstring ptr
extern p as const wstring ptr
extern array as const wstring * 10
extern array(0 to 9) as const wstring * 20

declare sub f(byval i as const wchar_t)
declare sub f(byval p as const wstring ptr)
declare sub f(byval array as const wstring ptr)

type UDT
	i as const wchar_t
	p as const wstring ptr
	array as const wstring * 10
	array(0 to 9) as const wstring * 20
end type

const A4 = cast(const wchar_t, 1)
const A5 = cptr(function() as const wchar_t, 1)
const A6 = cptr(sub(byval as const wchar_t), 1)

end extern
