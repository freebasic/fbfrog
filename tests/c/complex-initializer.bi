#pragma once

' Initializer parsing cannot easily distinguish between struct & array initializers,
' which look the same in C but have different syntax in FB.

type Vector2A
	x as long
	y as long
end type

dim shared a(0 to 3) as Vector2A = {(0, 0), (1, 0), (0, 1), (1, 1)}

type Vector2B
	coord(0 to 1) as long
end type

dim shared b(0 to 3) as Vector2B = {(0, 0), (1, 0), (0, 1), (1, 1)}
