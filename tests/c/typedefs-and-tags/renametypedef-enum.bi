#pragma once

'' The following symbols have been renamed:
''     typedef A => myint

type A as long
enum
	FOO
end enum

type myint as long
dim shared x1 as A
dim shared x2 as myint
