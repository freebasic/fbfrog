extern "C"

declare sub Toaster1()
declare sub toaster1_ alias "toaster1"()
declare sub toASTER1__ alias "toASTER1"()

#define AAA() 1
#define Aaa_() 2
#define aaa__() 3

const BBB = 1
const Bbb_ = 2
const bbb__ = 3

type myInt as long
type MyInt_ as long
type myint__ as long
type Myint___ as long

enum
	EnumConst1
	ENUMCONST1_
end enum

type Foo
	FiElD_ as long
	field__ as long
end type

type FOO_ as Foo

#define SAILINGBOAT_(x) sailingBoat(x + 9000)

declare sub sailingBoat(byval as long)

end extern
