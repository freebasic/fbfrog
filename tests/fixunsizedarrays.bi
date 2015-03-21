#pragma once

extern "C"

#define array1(i) ((@__array1)[i])
extern __array1 alias "array1" as long
extern array2(0 to 9) as long
#define array3(i) ((@__array3)[i])
dim shared __array3 as long
dim shared array4(0 to 9) as long
#define string(i) ((@__string)[i])
extern __string alias "string" as long
extern __zstring1 alias "zstring1" as byte
#define zstring1 (*cptr(zstring ptr, @__zstring1))
extern __wstring1 alias "wstring1" as wchar_t
#define wstring1 (*cptr(wstring ptr, @__wstring1))
extern __constzstring1 alias "constzstring1" as const byte
#define constzstring1 (*cptr(const zstring ptr, @__constzstring1))
extern __constwstring1 alias "constwstring1" as const wchar_t
#define constwstring1 (*cptr(const wstring ptr, @__constwstring1))

end extern
