#pragma once

extern "C"

extern directByteArrayToString as zstring * 10
extern directByteArrayLeftAsIs(0 to 9) as byte
extern directUbyteArrayToString as zstring * 10
extern directUbyteArrayLeftAsIs(0 to 9) as ubyte
type mybyte as ubyte
type myubyte as ubyte
extern indirectByteArrayToString as zstring * 10
extern indirectByteArrayLeftAsIs(0 to 9) as mybyte
extern indirectUbyteArrayToString as zstring * 10
extern indirectUbyteArrayLeftAsIs(0 to 9) as myubyte

declare sub f1(byval p1 as zstring ptr, byval p2 as ubyte ptr)
declare sub f2(byval p3 as byte ptr, byval p4 as zstring ptr)
declare sub f3(byval p5 as zstring ptr)
type CHAR as byte
extern singleChar as CHAR
extern charArray as zstring * 10
type CHAR2 as CHAR

type charUDT
	bufferField(0 to 9) as CHAR2
	stringField as zstring * 10
end type

type WCHAR as wchar_t
extern singleWChar as WCHAR
extern wcharArray as wstring * 10
type WCHAR2 as WCHAR

type wcharUDT
	bufferField(0 to 9) as WCHAR2
	stringField as wstring * 10
end type

end extern
