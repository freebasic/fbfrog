#pragma once

'' The following symbols have been renamed:
''     enum A => B

type B as long
enum
	FOO
end enum

type A as long
dim shared x1 as B
dim shared x2 as A
