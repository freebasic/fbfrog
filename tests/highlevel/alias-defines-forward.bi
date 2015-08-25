#pragma once

extern "C"

const C1 = 123
const C2 = C1
const C3 = C2
const C4 = C3
const C5 = C4
const C6 = C4
const C7 = C4

enum
	E1 = 123
end enum

const E2 = E1
type T2 as T1
type T1 as long
declare sub F1()
declare sub F2 alias "F1"()

#ifdef __FB_WIN32__
	declare function F3 stdcall alias "__F3__"(byval as double, byval as single) as long
	declare function F4 stdcall alias "__F3__"(byval as double, byval as single) as long
#else
	declare function F3 alias "__F3__"(byval as double, byval as single) as long
	declare function F4 alias "__F3__"(byval as double, byval as single) as long
#endif

type Struct2_ as Struct1

type Struct1
	dummy as long
end type

type Struct2 as Struct1
type Union2_ as Union1

union Union1
	dummy as long
end union

type Union2 as Union1
type Enum2_ as Enum1

type Enum1 as long
enum
	Enum1_dummy = 0
end enum

type Enum2 as Enum1
extern     V1 as long
dim shared V1 as long
extern     V2 alias "V1" as long
dim shared V2 as long
extern EV1 as long
extern EV2 alias "EV1" as long
extern EV3 alias "__EV3__" as long
extern EV4 alias "__EV3__" as long

end extern
