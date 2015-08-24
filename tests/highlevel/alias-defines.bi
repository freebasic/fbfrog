#pragma once

extern "C"

const C1 = 123
const C2 = C1

enum
	E1 = 123
end enum

const E2 = E1
type T1 as long
type T2 as T1
declare sub F1()

#ifdef __FB_WIN32__
	declare function F3 alias "__F3__" stdcall(byval as double, byval as single) as long
#else
	declare function F3 alias "__F3__"(byval as double, byval as single) as long
#endif

declare sub F2 alias "F1"()

#ifdef __FB_WIN32__
	declare function F4 alias "__F3__" stdcall(byval as double, byval as single) as long
#else
	declare function F4 alias "__F3__"(byval as double, byval as single) as long
#endif

type Struct1
	dummy as long
end type

type Struct2 as Struct1
type Struct2_ as Struct1

union Union1
	dummy as long
end union

type Union2 as Union1
type Union2_ as Union1

type Enum1 as long
enum
	Enum1_dummy = 0
end enum

type Enum2 as Enum1
type Enum2_ as Enum1

end extern
