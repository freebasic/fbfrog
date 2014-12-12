#pragma once

'' The following symbols have been renamed:
''     procedure toaster1 => toaster1_
''     procedure toASTER1 => toASTER1__
''     #define Aaa => Aaa_
''     #define aaa => aaa__
''     #define Bbb => Bbb_
''     #define bbb => bbb__
''     typedef MyInt => MyInt_
''     typedef myint => myint__
''     typedef Myint => Myint___
''     enum constant ENUMCONST1 => ENUMCONST1_
''     inside struct Foo:
''         field field => field_
''     typedef FOO => FOO_
''     #define SAILINGBOAT => SAILINGBOAT_

extern "C"

declare sub Toaster1()
declare sub toaster1_ alias "toaster1"()
declare sub toASTER1__ alias "toASTER1"()

#define AAA() 1
#define Aaa_() 2
#define aaa__() 3
#define BBB 1
#define Bbb_ 2
#define bbb__ 3

type myInt as long
type MyInt_ as long
type myint__ as long
type Myint___ as long

enum
	EnumConst1
	ENUMCONST1_
end enum

type Foo
	FiElD as long
	field_ as long
end type

type FOO_ as Foo

#define SAILINGBOAT_(x) sailingBoat(x + 9000)

declare sub sailingBoat(byval as long)

end extern
